//! Document management for the LSP server.

use dashmap::DashMap;
use tower_lsp::lsp_types::*;
use vfp_lexer::{Token, TokenKind, tokenize};

/// A document being edited.
#[derive(Debug)]
pub struct Document {
    /// The document content.
    pub content: String,
    /// Tokens from lexing.
    pub tokens: Vec<Token>,
    /// Line start offsets for position conversion.
    line_starts: Vec<usize>,
}

impl Document {
    /// Create a new document from content.
    pub fn new(content: String) -> Self {
        let tokens = tokenize(&content).collect();
        let line_starts = Self::compute_line_starts(&content);
        Self {
            content,
            tokens,
            line_starts,
        }
    }

    /// Update the document content.
    pub fn update(&mut self, content: String) {
        self.tokens = tokenize(&content).collect();
        self.line_starts = Self::compute_line_starts(&content);
        self.content = content;
    }

    /// Compute line start offsets.
    fn compute_line_starts(content: &str) -> Vec<usize> {
        let mut starts = vec![0];
        for (i, c) in content.char_indices() {
            if c == '\n' {
                starts.push(i + 1);
            }
        }
        starts
    }

    /// Convert a byte offset to a Position.
    pub fn offset_to_position(&self, offset: usize) -> Position {
        let line = self
            .line_starts
            .partition_point(|&start| start <= offset)
            .saturating_sub(1);
        let line_start = self.line_starts[line];
        let character = self.content[line_start..offset].chars().count() as u32;
        Position::new(line as u32, character)
    }

    /// Convert a Position to a byte offset.
    pub fn position_to_offset(&self, position: Position) -> usize {
        let line = position.line as usize;
        if line >= self.line_starts.len() {
            return self.content.len();
        }
        let line_start = self.line_starts[line];
        let line_content = if line + 1 < self.line_starts.len() {
            &self.content[line_start..self.line_starts[line + 1]]
        } else {
            &self.content[line_start..]
        };

        let mut offset = line_start;
        for (i, c) in line_content.char_indices() {
            if i as u32 >= position.character {
                break;
            }
            offset = line_start + i + c.len_utf8();
        }
        offset.min(self.content.len())
    }

    /// Get diagnostics for the document.
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        let mut offset = 0;

        for token in &self.tokens {
            match token.kind {
                TokenKind::Unknown => {
                    let start = self.offset_to_position(offset);
                    let end = self.offset_to_position(offset + token.len as usize);
                    diagnostics.push(Diagnostic {
                        range: Range::new(start, end),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("vfp-lsp".to_string()),
                        message: "Unknown character".to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
                TokenKind::Literal {
                    kind: vfp_lexer::LiteralKind::StringDouble { terminated: false },
                }
                | TokenKind::Literal {
                    kind: vfp_lexer::LiteralKind::StringSingle { terminated: false },
                }
                | TokenKind::Literal {
                    kind: vfp_lexer::LiteralKind::StringBracket { terminated: false },
                } => {
                    let start = self.offset_to_position(offset);
                    let end = self.offset_to_position(offset + token.len as usize);
                    diagnostics.push(Diagnostic {
                        range: Range::new(start, end),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("vfp-lsp".to_string()),
                        message: "Unterminated string literal".to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
                _ => {}
            }
            offset += token.len as usize;
        }

        // Add semantic validation
        diagnostics.extend(self.semantic_diagnostics());

        // Add block structure validation
        diagnostics.extend(self.block_diagnostics());

        // Add duplicate definition detection
        diagnostics.extend(self.duplicate_diagnostics());

        diagnostics
    }

    /// Semantic validation with fuzzy keyword matching.
    fn semantic_diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        let mut offset = 0;

        for token in &self.tokens {
            if token.kind == TokenKind::Ident {
                let text = &self.content[offset..offset + token.len as usize];
                let upper = text.to_ascii_uppercase();

                // Skip if it's a valid keyword
                if !vfp_lexer::is_keyword(&upper) && !vfp_lexer::is_sql_keyword(&upper) {
                    // Check for possible typos
                    if let Some(suggestion) = find_similar_keyword(&upper) {
                        let start = self.offset_to_position(offset);
                        let end = self.offset_to_position(offset + token.len as usize);
                        diagnostics.push(Diagnostic {
                            range: Range::new(start, end),
                            severity: Some(DiagnosticSeverity::WARNING),
                            code: None,
                            code_description: None,
                            source: Some("vfp-lsp".to_string()),
                            message: format!(
                                "Possible misspelling. Did you mean '{}'?",
                                suggestion
                            ),
                            related_information: None,
                            tags: None,
                            data: None,
                        });
                    }
                }
            }

            offset += token.len as usize;
        }

        diagnostics
    }

    /// Block structure validation - checks for unclosed blocks.
    fn block_diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        let mut block_stack: Vec<(String, usize, Position)> = Vec::new();
        let mut offset = 0;
        let mut in_text_merge = false;
        let mut in_preprocessor_line = false;
        let mut in_text_block = false;

        for token in &self.tokens {
            // Skip comments entirely - they can contain words like "for" or "if"
            if token.kind.is_comment() {
                offset += token.len as usize;
                continue;
            }

            // Skip string literals - they can contain keywords too
            if matches!(token.kind, TokenKind::Literal { .. }) {
                offset += token.len as usize;
                continue;
            }

            // Track preprocessor directives - don't parse keywords inside them
            if token.kind.is_preprocessor() {
                in_preprocessor_line = true;
                offset += token.len as usize;
                continue;
            }

            // End of preprocessor line
            if in_preprocessor_line && token.kind == TokenKind::Newline {
                in_preprocessor_line = false;
                offset += token.len as usize;
                continue;
            }

            // Skip parsing inside preprocessor directives
            if in_preprocessor_line {
                offset += token.len as usize;
                continue;
            }

            // Track text merge regions - don't parse inside <<...>>
            if token.kind == TokenKind::TextMergeOpen {
                in_text_merge = true;
                offset += token.len as usize;
                continue;
            }
            if token.kind == TokenKind::TextMergeClose {
                in_text_merge = false;
                offset += token.len as usize;
                continue;
            }

            // Skip parsing inside text merge expressions
            if in_text_merge {
                offset += token.len as usize;
                continue;
            }

            if token.kind == TokenKind::Ident {
                let text = &self.content[offset..offset + token.len as usize];
                let upper = text.to_ascii_uppercase();
                let pos = self.offset_to_position(offset);

                // Check for TEXT block start/end
                if upper == "TEXT" && !in_text_block {
                    block_stack.push(("TEXT".to_string(), offset, pos));
                    in_text_block = true;
                    offset += token.len as usize;
                    continue;
                }

                if upper == "ENDTEXT" && in_text_block {
                    if close_block(&mut block_stack, "TEXT", &mut diagnostics, offset, self) {
                        in_text_block = false;
                    } else {
                        diagnostics.push(make_error(
                            "ENDTEXT without matching TEXT",
                            offset,
                            token.len as usize,
                            self,
                        ));
                    }
                    offset += token.len as usize;
                    continue;
                }

                // Skip parsing keywords inside TEXT blocks
                if in_text_block {
                    offset += token.len as usize;
                    continue;
                }

                match upper.as_str() {
                    // Block openers
                    "IF" => block_stack.push(("IF".to_string(), offset, pos)),
                    "FOR" => block_stack.push(("FOR".to_string(), offset, pos)),
                    "WHILE" => {
                        // Check if previous was DO_TENTATIVE -> make it DO_WHILE
                        if let Some((kind, _, _)) = block_stack.last_mut() {
                            if kind == "DO_TENTATIVE" {
                                *kind = "DO_WHILE".to_string();
                            } else {
                                // Standalone WHILE block
                                block_stack.push(("WHILE".to_string(), offset, pos));
                            }
                        } else {
                            // Standalone WHILE block
                            block_stack.push(("WHILE".to_string(), offset, pos));
                        }
                    }
                    "DO" => {
                        // DO is only a block if followed by WHILE or CASE
                        // DO procedure calls are not blocks
                        // We'll mark it tentatively and remove if not followed by WHILE/CASE
                        block_stack.push(("DO_TENTATIVE".to_string(), offset, pos));
                    }
                    "SCAN" => block_stack.push(("SCAN".to_string(), offset, pos)),
                    "TRY" => block_stack.push(("TRY".to_string(), offset, pos)),
                    "WITH" => block_stack.push(("WITH".to_string(), offset, pos)),
                    "CASE" => {
                        // If previous was DO_TENTATIVE, convert to DO_CASE
                        if let Some((kind, _, _)) = block_stack.last_mut() {
                            if kind == "DO_TENTATIVE" {
                                *kind = "DO_CASE".to_string();
                            }
                        }
                    }
                    "FUNCTION" => block_stack.push(("FUNCTION".to_string(), offset, pos)),
                    "PROCEDURE" => block_stack.push(("PROCEDURE".to_string(), offset, pos)),
                    "DEFINE" => block_stack.push(("DEFINE".to_string(), offset, pos)),

                    // Block closers
                    "ENDIF" => {
                        if !close_block(&mut block_stack, "IF", &mut diagnostics, offset, self) {
                            diagnostics.push(make_error(
                                "ENDIF without matching IF",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDFOR" | "NEXT" => {
                        if !close_block(&mut block_stack, "FOR", &mut diagnostics, offset, self) {
                            diagnostics.push(make_error(
                                "ENDFOR without matching FOR",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDDO" => {
                        // Can close DO_WHILE or WHILE (not DO_TENTATIVE - those are procedure calls)
                        let closed = close_block(
                            &mut block_stack,
                            "DO_WHILE",
                            &mut diagnostics,
                            offset,
                            self,
                        ) || close_block(
                            &mut block_stack,
                            "WHILE",
                            &mut diagnostics,
                            offset,
                            self,
                        );
                        if !closed {
                            diagnostics.push(make_error(
                                "ENDDO without matching DO WHILE/WHILE",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDWHILE" => {
                        if !close_block(&mut block_stack, "WHILE", &mut diagnostics, offset, self) {
                            diagnostics.push(make_error(
                                "ENDWHILE without matching WHILE",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDSCAN" => {
                        if !close_block(&mut block_stack, "SCAN", &mut diagnostics, offset, self) {
                            diagnostics.push(make_error(
                                "ENDSCAN without matching SCAN",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDCASE" => {
                        if !close_block(&mut block_stack, "DO_CASE", &mut diagnostics, offset, self)
                        {
                            diagnostics.push(make_error(
                                "ENDCASE without matching DO CASE",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDTRY" => {
                        if !close_block(&mut block_stack, "TRY", &mut diagnostics, offset, self) {
                            diagnostics.push(make_error(
                                "ENDTRY without matching TRY",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDWITH" => {
                        if !close_block(&mut block_stack, "WITH", &mut diagnostics, offset, self) {
                            diagnostics.push(make_error(
                                "ENDWITH without matching WITH",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDFUNC" => {
                        if !close_block(
                            &mut block_stack,
                            "FUNCTION",
                            &mut diagnostics,
                            offset,
                            self,
                        ) {
                            diagnostics.push(make_error(
                                "ENDFUNC without matching FUNCTION",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDPROC" => {
                        if !close_block(
                            &mut block_stack,
                            "PROCEDURE",
                            &mut diagnostics,
                            offset,
                            self,
                        ) {
                            diagnostics.push(make_error(
                                "ENDPROC without matching PROCEDURE",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    "ENDDEFINE" => {
                        if !close_block(&mut block_stack, "DEFINE", &mut diagnostics, offset, self)
                        {
                            diagnostics.push(make_error(
                                "ENDDEFINE without matching DEFINE CLASS",
                                offset,
                                token.len as usize,
                                self,
                            ));
                        }
                    }
                    _ => {}
                }
            }

            offset += token.len as usize;
        }

        // Report unclosed blocks (skip DO_TENTATIVE - those are procedure calls)
        for (kind, _, pos) in block_stack {
            // Skip tentative DO - it's a procedure call, not a block
            if kind == "DO_TENTATIVE" {
                continue;
            }

            let expected = match kind.as_str() {
                "IF" => "ENDIF",
                "FOR" => "ENDFOR",
                "WHILE" => "ENDWHILE or ENDDO",
                "DO_CASE" => "ENDCASE",
                "DO_WHILE" => "ENDDO",
                "SCAN" => "ENDSCAN",
                "TRY" => "ENDTRY",
                "WITH" => "ENDWITH",
                "TEXT" => "ENDTEXT",
                "FUNCTION" => "ENDFUNC",
                "PROCEDURE" => "ENDPROC",
                "DEFINE" => "ENDDEFINE",
                _ => continue, // Skip unknown block types
            };
            diagnostics.push(Diagnostic {
                range: Range::new(pos, pos),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("vfp-lsp".to_string()),
                message: format!("Unclosed {}. Expected {}", kind.replace('_', " "), expected),
                related_information: None,
                tags: None,
                data: None,
            });
        }

        diagnostics
    }

    /// Duplicate definition detection.
    fn duplicate_diagnostics(&self) -> Vec<Diagnostic> {
        use std::collections::HashMap;

        let mut diagnostics = Vec::new();
        // Map from name -> (kind, offset, position)
        let mut definitions: HashMap<String, Vec<(String, usize, Position)>> = HashMap::new();
        let mut offset = 0;
        let mut i = 0;

        while i < self.tokens.len() {
            let token = &self.tokens[i];

            if token.kind == TokenKind::Ident {
                let text = &self.content[offset..offset + token.len as usize];
                let upper = text.to_ascii_uppercase();

                match upper.as_str() {
                    "FUNCTION" | "PROCEDURE" => {
                        // Next non-trivia token is the name
                        let mut j = i + 1;
                        let mut name_offset = offset + token.len as usize;

                        while j < self.tokens.len() {
                            let next = &self.tokens[j];
                            if !next.kind.is_trivia() {
                                if next.kind == TokenKind::Ident {
                                    let name =
                                        &self.content[name_offset..name_offset + next.len as usize];
                                    let name_upper = name.to_ascii_uppercase();
                                    let pos = self.offset_to_position(name_offset);
                                    definitions.entry(name_upper).or_default().push((
                                        upper.clone(),
                                        name_offset,
                                        pos,
                                    ));
                                }
                                break;
                            }
                            name_offset += next.len as usize;
                            j += 1;
                        }
                    }
                    "DEFINE" => {
                        // Look for CLASS keyword then name
                        let mut j = i + 1;
                        let mut class_offset = offset + token.len as usize;

                        while j < self.tokens.len() {
                            let next = &self.tokens[j];
                            if !next.kind.is_trivia() {
                                let next_text =
                                    &self.content[class_offset..class_offset + next.len as usize];
                                if next_text.eq_ignore_ascii_case("CLASS") {
                                    // Next token is the class name
                                    let mut k = j + 1;
                                    let mut name_offset = class_offset + next.len as usize;

                                    while k < self.tokens.len() {
                                        let name_token = &self.tokens[k];
                                        if !name_token.kind.is_trivia() {
                                            if name_token.kind == TokenKind::Ident {
                                                let name = &self.content[name_offset
                                                    ..name_offset + name_token.len as usize];
                                                let name_upper = name.to_ascii_uppercase();
                                                let pos = self.offset_to_position(name_offset);
                                                definitions.entry(name_upper).or_default().push((
                                                    "CLASS".to_string(),
                                                    name_offset,
                                                    pos,
                                                ));
                                            }
                                            break;
                                        }
                                        name_offset += name_token.len as usize;
                                        k += 1;
                                    }
                                }
                                break;
                            }
                            class_offset += next.len as usize;
                            j += 1;
                        }
                    }
                    _ => {}
                }
            }

            offset += token.len as usize;
            i += 1;
        }

        // Report duplicates
        for (name, defs) in definitions {
            if defs.len() > 1 {
                for (kind, def_offset, pos) in &defs[1..] {
                    let first_kind = &defs[0].0;
                    let first_pos = &defs[0].2;
                    let name_in_content = &self.content[*def_offset..*def_offset + name.len()];
                    diagnostics.push(Diagnostic {
                        range: Range::new(
                            *pos,
                            Position::new(pos.line, pos.character + name.len() as u32),
                        ),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("vfp-lsp".to_string()),
                        message: format!(
                            "Duplicate {} '{}'. First defined as {} at line {}",
                            kind,
                            name_in_content,
                            first_kind,
                            first_pos.line + 1
                        ),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
        }

        diagnostics
    }

    /// Get the token at a given offset.
    pub fn token_at_offset(&self, offset: usize) -> Option<(&Token, usize)> {
        let mut pos = 0;
        for token in &self.tokens {
            let token_end = pos + token.len as usize;
            if offset >= pos && offset < token_end {
                return Some((token, pos));
            }
            pos = token_end;
        }
        None
    }

    /// Get the word at a given position.
    pub fn word_at_position(&self, position: Position) -> Option<String> {
        let offset = self.position_to_offset(position);
        let (token, start) = self.token_at_offset(offset)?;

        if token.kind == TokenKind::Ident {
            let text = &self.content[start..start + token.len as usize];
            return Some(text.to_string());
        }

        None
    }

    /// Get all symbols in the document.
    pub fn symbols(&self) -> Vec<DocumentSymbol> {
        let mut symbols = Vec::new();
        let mut offset = 0;
        let mut i = 0;

        while i < self.tokens.len() {
            let token = &self.tokens[i];

            // Look for FUNCTION, PROCEDURE, CLASS definitions
            if token.kind == TokenKind::Ident {
                let text = &self.content[offset..offset + token.len as usize];
                let upper = text.to_ascii_uppercase();

                match upper.as_str() {
                    "FUNCTION" | "PROCEDURE" => {
                        // Next non-trivia token should be the name
                        let start_offset = offset;
                        let mut name_offset = offset + token.len as usize;
                        let mut j = i + 1;

                        while j < self.tokens.len() {
                            let next = &self.tokens[j];
                            if !next.kind.is_trivia() {
                                if next.kind == TokenKind::Ident {
                                    let name =
                                        &self.content[name_offset..name_offset + next.len as usize];
                                    let start = self.offset_to_position(start_offset);
                                    let end =
                                        self.offset_to_position(name_offset + next.len as usize);

                                    #[allow(deprecated)]
                                    symbols.push(DocumentSymbol {
                                        name: name.to_string(),
                                        detail: Some(format!("{} {}", upper, name)),
                                        kind: if upper == "FUNCTION" {
                                            SymbolKind::FUNCTION
                                        } else {
                                            SymbolKind::METHOD
                                        },
                                        tags: None,
                                        deprecated: None,
                                        range: Range::new(start, end),
                                        selection_range: Range::new(
                                            self.offset_to_position(name_offset),
                                            self.offset_to_position(
                                                name_offset + next.len as usize,
                                            ),
                                        ),
                                        children: None,
                                    });
                                }
                                break;
                            }
                            name_offset += next.len as usize;
                            j += 1;
                        }
                    }
                    "DEFINE" => {
                        // Look for CLASS keyword
                        let mut class_offset = offset + token.len as usize;
                        let mut j = i + 1;

                        while j < self.tokens.len() {
                            let next = &self.tokens[j];
                            if !next.kind.is_trivia() {
                                let next_text =
                                    &self.content[class_offset..class_offset + next.len as usize];
                                if next_text.eq_ignore_ascii_case("CLASS") {
                                    // Next token is the class name
                                    let mut name_offset = class_offset + next.len as usize;
                                    let mut k = j + 1;

                                    while k < self.tokens.len() {
                                        let name_token = &self.tokens[k];
                                        if !name_token.kind.is_trivia() {
                                            if name_token.kind == TokenKind::Ident {
                                                let name = &self.content[name_offset
                                                    ..name_offset + name_token.len as usize];
                                                let start = self.offset_to_position(offset);
                                                let end = self.offset_to_position(
                                                    name_offset + name_token.len as usize,
                                                );

                                                #[allow(deprecated)]
                                                symbols.push(DocumentSymbol {
                                                    name: name.to_string(),
                                                    detail: Some(format!("CLASS {}", name)),
                                                    kind: SymbolKind::CLASS,
                                                    tags: None,
                                                    deprecated: None,
                                                    range: Range::new(start, end),
                                                    selection_range: Range::new(
                                                        self.offset_to_position(name_offset),
                                                        self.offset_to_position(
                                                            name_offset + name_token.len as usize,
                                                        ),
                                                    ),
                                                    children: None,
                                                });
                                            }
                                            break;
                                        }
                                        name_offset += name_token.len as usize;
                                        k += 1;
                                    }
                                }
                                break;
                            }
                            class_offset += next.len as usize;
                            j += 1;
                        }
                    }
                    _ => {}
                }
            }

            offset += token.len as usize;
            i += 1;
        }

        symbols
    }

    /// Find definition of a symbol (FUNCTION, PROCEDURE, CLASS) by name
    pub fn find_definition(&self, name: &str) -> Option<Range> {
        let mut offset = 0;
        let mut i = 0;

        while i < self.tokens.len() {
            let token = &self.tokens[i];

            if token.kind == TokenKind::Ident {
                let text = &self.content[offset..offset + token.len as usize];
                let upper = text.to_ascii_uppercase();

                match upper.as_str() {
                    "FUNCTION" | "PROCEDURE" => {
                        // Look ahead for the function/procedure name
                        let mut j = i + 1;
                        let mut name_offset = offset + token.len as usize;

                        while j < self.tokens.len() {
                            let next = &self.tokens[j];
                            if !next.kind.is_trivia() {
                                if next.kind == TokenKind::Ident {
                                    let func_name =
                                        &self.content[name_offset..name_offset + next.len as usize];
                                    if func_name.eq_ignore_ascii_case(name) {
                                        let start = self.offset_to_position(name_offset);
                                        let end = self
                                            .offset_to_position(name_offset + next.len as usize);
                                        return Some(Range::new(start, end));
                                    }
                                }
                                break;
                            }
                            name_offset += next.len as usize;
                            j += 1;
                        }
                    }
                    "DEFINE" => {
                        // Look for CLASS keyword, then name
                        let mut j = i + 1;
                        let mut class_offset = offset + token.len as usize;

                        while j < self.tokens.len() {
                            let next = &self.tokens[j];
                            if !next.kind.is_trivia() {
                                let next_text =
                                    &self.content[class_offset..class_offset + next.len as usize];
                                if next_text.eq_ignore_ascii_case("CLASS") {
                                    // Next token is the class name
                                    let mut k = j + 1;
                                    let mut name_offset = class_offset + next.len as usize;

                                    while k < self.tokens.len() {
                                        let name_token = &self.tokens[k];
                                        if !name_token.kind.is_trivia() {
                                            if name_token.kind == TokenKind::Ident {
                                                let class_name = &self.content[name_offset
                                                    ..name_offset + name_token.len as usize];
                                                if class_name.eq_ignore_ascii_case(name) {
                                                    let start =
                                                        self.offset_to_position(name_offset);
                                                    let end = self.offset_to_position(
                                                        name_offset + name_token.len as usize,
                                                    );
                                                    return Some(Range::new(start, end));
                                                }
                                            }
                                            break;
                                        }
                                        name_offset += name_token.len as usize;
                                        k += 1;
                                    }
                                }
                                break;
                            }
                            class_offset += next.len as usize;
                            j += 1;
                        }
                    }
                    _ => {}
                }
            }

            offset += token.len as usize;
            i += 1;
        }

        None
    }
}

/// Store for all open documents.
pub struct DocumentStore {
    documents: DashMap<Url, Document>,
}

impl DocumentStore {
    pub fn new() -> Self {
        Self {
            documents: DashMap::new(),
        }
    }

    pub fn open(&self, uri: Url, content: String) {
        self.documents.insert(uri, Document::new(content));
    }

    pub fn update(&self, uri: &Url, content: String) {
        if let Some(mut doc) = self.documents.get_mut(uri) {
            doc.update(content);
        }
    }

    pub fn close(&self, uri: &Url) {
        self.documents.remove(uri);
    }

    pub fn get(&self, uri: &Url) -> Option<dashmap::mapref::one::Ref<'_, Url, Document>> {
        self.documents.get(uri)
    }
}

impl Default for DocumentStore {
    fn default() -> Self {
        Self::new()
    }
}

/// Find a similar keyword if the word looks like a typo.
/// Currently disabled - too many false positives with VFP naming conventions.
fn find_similar_keyword(_word: &str) -> Option<&'static str> {
    None
}

/// Try to close a block of the given type.
fn close_block(
    stack: &mut Vec<(String, usize, Position)>,
    expected: &str,
    _diagnostics: &mut Vec<Diagnostic>,
    _offset: usize,
    _doc: &Document,
) -> bool {
    // Find the most recent matching block
    for i in (0..stack.len()).rev() {
        if stack[i].0 == expected {
            stack.remove(i);
            return true;
        }
    }
    false
}

/// Create an error diagnostic.
fn make_error(message: &str, offset: usize, len: usize, doc: &Document) -> Diagnostic {
    let start = doc.offset_to_position(offset);
    let end = doc.offset_to_position(offset + len);
    Diagnostic {
        range: Range::new(start, end),
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("vfp-lsp".to_string()),
        message: message.to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strings_with_backslashes() {
        // VFP doesn't use backslash escapes - backslashes are literal
        // This is important for file paths like "C:\Data\"
        let doc = Document::new(r#"#DEFINE DATA_PATH "C:\Data\""#.to_string());
        let diagnostics = doc.diagnostics();

        // Should have no errors - the string is properly terminated
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("Unterminated"))
            .collect();
        assert_eq!(
            errors.len(),
            0,
            "Should not report unterminated string for backslash in path"
        );
    }

    #[test]
    fn test_preprocessor_with_if_keyword() {
        // Keywords inside #DEFINE should not trigger block validation
        let code = r#"
#DEFINE ASSERT(cond) IF .NOT. (cond) THEN MESSAGEBOX("Failed")
FUNCTION Test
ENDFUNCTION
"#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

        // Should have no errors - IF inside #DEFINE is not a block structure
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("ENDIF"))
            .collect();
        assert_eq!(
            errors.len(),
            0,
            "Should not report unclosed IF for keywords inside #DEFINE"
        );
    }

    #[test]
    fn test_preprocessor_string_backslash() {
        // Test the exact case from the bug report
        let code = r#"#DEFINE PATH_SEPARATOR "\""#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

        // Should have no errors
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("Unterminated"))
            .collect();
        assert_eq!(
            errors.len(),
            0,
            "Should not report unterminated string for backslash separator"
        );
    }

    #[test]
    fn test_real_unclosed_block() {
        // Make sure we still catch real errors
        let code = "IF .T.\n    ? \"Hello\"\n* Missing ENDIF\n";
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

        // Should report the missing ENDIF
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("IF") || d.message.contains("ENDIF"))
            .collect();
        assert!(
            !errors.is_empty(),
            "Should still catch real unclosed IF blocks"
        );
    }

    #[test]
    fn test_keywords_in_text_blocks() {
        // Keywords inside TEXT blocks should not trigger validation
        let code = r#"
TEXT TO cOutput
Thank you for your order.
If you have questions, please contact us.
ENDTEXT
"#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

        // Should have no errors - "for" and "If" are just text
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("IF") || d.message.contains("FOR"))
            .collect();
        assert_eq!(
            errors.len(),
            0,
            "Should not report keywords inside TEXT blocks"
        );
    }

    #[test]
    fn test_keywords_in_comments() {
        // Keywords in comments should not trigger validation
        let code = r#"
* This is for future use
* If we need to add more features
FUNCTION Test
ENDFUNC
"#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

        // Should have no errors - "for" and "If" are in comments
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("IF") || d.message.contains("FOR"))
            .collect();
        assert_eq!(
            errors.len(),
            0,
            "Should not report keywords inside comments"
        );
    }

    #[test]
    fn test_keywords_in_strings() {
        // Keywords in strings should not trigger validation
        let code = r#"
LOCAL cMsg
cMsg = "This is for testing if strings work"
? "For each item in the list"
"#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

        // Should have no errors - "for" and "if" are in strings
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("IF") || d.message.contains("FOR"))
            .collect();
        assert_eq!(errors.len(), 0, "Should not report keywords inside strings");
    }
}
