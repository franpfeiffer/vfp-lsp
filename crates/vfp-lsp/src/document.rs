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

        diagnostics.extend(self.semantic_diagnostics());
        diagnostics.extend(self.block_diagnostics());
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

                if !vfp_lexer::is_keyword(&upper) && !vfp_lexer::is_sql_keyword(&upper) {
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

    fn block_diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        let mut block_stack: Vec<(String, usize, Position)> = Vec::new();
        let mut offset = 0;
        let mut in_text_merge = false;
        let mut in_preprocessor_line = false;
        let mut in_text_block = false;

        for token in &self.tokens {
            if token.kind.is_comment() {
                offset += token.len as usize;
                continue;
            }

            if matches!(token.kind, TokenKind::Literal { .. }) {
                offset += token.len as usize;
                continue;
            }

            if token.kind.is_preprocessor() {
                in_preprocessor_line = true;
                offset += token.len as usize;
                continue;
            }

            if in_preprocessor_line && token.kind == TokenKind::Newline {
                in_preprocessor_line = false;
                offset += token.len as usize;
                continue;
            }

            if in_preprocessor_line {
                offset += token.len as usize;
                continue;
            }

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

            if in_text_merge {
                offset += token.len as usize;
                continue;
            }

            if token.kind == TokenKind::Ident {
                let text = &self.content[offset..offset + token.len as usize];
                let upper = text.to_ascii_uppercase();
                let pos = self.offset_to_position(offset);

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

                if in_text_block {
                    offset += token.len as usize;
                    continue;
                }

                match upper.as_str() {
                    "IF" => block_stack.push(("IF".to_string(), offset, pos)),
                    "FOR" => block_stack.push(("FOR".to_string(), offset, pos)),
                    "WHILE" => {
                        if let Some((kind, _, _)) = block_stack.last_mut() {
                            if kind == "DO_TENTATIVE" {
                                *kind = "DO_WHILE".to_string();
                            } else {
                                block_stack.push(("WHILE".to_string(), offset, pos));
                            }
                        } else {
                            block_stack.push(("WHILE".to_string(), offset, pos));
                        }
                    }
                    "DO" => {
                        block_stack.push(("DO_TENTATIVE".to_string(), offset, pos));
                    }
                    "SCAN" => block_stack.push(("SCAN".to_string(), offset, pos)),
                    "TRY" => block_stack.push(("TRY".to_string(), offset, pos)),
                    "WITH" => block_stack.push(("WITH".to_string(), offset, pos)),
                    "CASE" => {
                        if let Some((kind, _, _)) = block_stack.last_mut() {
                            if kind == "DO_TENTATIVE" {
                                *kind = "DO_CASE".to_string();
                            }
                        }
                    }
                    "FUNCTION" => block_stack.push(("FUNCTION".to_string(), offset, pos)),
                    "PROCEDURE" => block_stack.push(("PROCEDURE".to_string(), offset, pos)),
                    "DEFINE" => block_stack.push(("DEFINE".to_string(), offset, pos)),

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

        for (kind, _, pos) in block_stack {
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
                _ => continue,
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

    fn duplicate_diagnostics(&self) -> Vec<Diagnostic> {
        use std::collections::HashMap;

        let mut diagnostics = Vec::new();
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

    pub fn symbols(&self) -> Vec<DocumentSymbol> {
        let mut symbols = Vec::new();
        let mut offset = 0;
        let mut i = 0;

        while i < self.tokens.len() {
            let token = &self.tokens[i];

            if token.kind == TokenKind::Ident {
                let text = &self.content[offset..offset + token.len as usize];
                let upper = text.to_ascii_uppercase();

                match upper.as_str() {
                    "FUNCTION" | "PROCEDURE" => {
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
                        let mut class_offset = offset + token.len as usize;
                        let mut j = i + 1;

                        while j < self.tokens.len() {
                            let next = &self.tokens[j];
                            if !next.kind.is_trivia() {
                                let next_text =
                                    &self.content[class_offset..class_offset + next.len as usize];
                                if next_text.eq_ignore_ascii_case("CLASS") {
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

    pub fn find_signature(&self, name: &str) -> Option<SignatureInformation> {
        let mut offset = 0;
        let mut i = 0;

        while i < self.tokens.len() {
            let token = &self.tokens[i];

            if token.kind == TokenKind::Ident {
                let text = &self.content[offset..offset + token.len as usize];
                let upper = text.to_ascii_uppercase();

                match upper.as_str() {
                    "FUNCTION" | "PROCEDURE" => {
                        let kind = upper.clone();
                        let mut j = i + 1;
                        let mut name_offset = offset + token.len as usize;

                        while j < self.tokens.len() {
                            let next = &self.tokens[j];
                            if !next.kind.is_trivia() {
                                if next.kind == TokenKind::Ident {
                                    let func_name =
                                        &self.content[name_offset..name_offset + next.len as usize];
                                    if func_name.eq_ignore_ascii_case(name) {
                                        return self.extract_signature(
                                            func_name,
                                            &kind,
                                            j + 1,
                                            name_offset + next.len as usize,
                                        );
                                    }
                                }
                                break;
                            }
                            name_offset += next.len as usize;
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

    fn extract_signature(
        &self,
        name: &str,
        kind: &str,
        token_idx: usize,
        mut offset: usize,
    ) -> Option<SignatureInformation> {
        let mut params = Vec::new();
        let mut i = token_idx;
        let mut found_paren = false;
        while i < self.tokens.len() {
            let token = &self.tokens[i];
            
            if token.kind == TokenKind::LParen {
                found_paren = true;
                i += 1;
                offset += token.len as usize;
                
                let mut param_start = None;
                while i < self.tokens.len() {
                    let token = &self.tokens[i];
                    
                    if token.kind == TokenKind::RParen {
                        if let Some(start) = param_start {
                            let param_text = self.content[start..offset].trim();
                            if !param_text.is_empty() {
                                params.push(param_text.to_string());
                            }
                        }
                        break;
                    } else if token.kind == TokenKind::Comma {
                        if let Some(start) = param_start {
                            let param_text = self.content[start..offset].trim();
                            if !param_text.is_empty() {
                                params.push(param_text.to_string());
                            }
                        }
                        param_start = None;
                    } else if token.kind == TokenKind::Ident && param_start.is_none() {
                        param_start = Some(offset);
                    }
                    
                    offset += token.len as usize;
                    i += 1;
                }
                break;
            } else if token.kind == TokenKind::Newline {
                break;
            }
            
            offset += token.len as usize;
            i += 1;
        }

        if !found_paren {
            while i < self.tokens.len() {
                let token = &self.tokens[i];

                if token.kind == TokenKind::Ident {
                    let text = &self.content[offset..offset + token.len as usize];
                    let upper = text.to_ascii_uppercase();

                    if upper == "LPARAMETERS" || upper == "PARAMETERS" {
                        i += 1;
                        offset += token.len as usize;

                        let mut param_start = None;
                        while i < self.tokens.len() {
                            let token = &self.tokens[i];

                            if token.kind == TokenKind::Newline {
                                if let Some(start) = param_start {
                                    let param_text = self.content[start..offset].trim();
                                    if !param_text.is_empty() {
                                        params.push(param_text.to_string());
                                    }
                                }
                                break;
                            } else if token.kind == TokenKind::Comma {
                                if let Some(start) = param_start {
                                    let param_text = self.content[start..offset].trim();
                                    if !param_text.is_empty() {
                                        params.push(param_text.to_string());
                                    }
                                }
                                param_start = None;
                            } else if token.kind == TokenKind::Ident && param_start.is_none() {
                                param_start = Some(offset);
                            }

                            offset += token.len as usize;
                            i += 1;
                        }
                        break;
                    } else if matches!(
                        upper.as_str(),
                        "FUNCTION" | "PROCEDURE" | "ENDFUNC" | "ENDPROC"
                    ) {
                        break;
                    }
                }

                offset += token.len as usize;
                i += 1;

                if i > token_idx + 50 {
                    break;
                }
            }
        }

        let param_list = if params.is_empty() {
            String::new()
        } else {
            params.join(", ")
        };

        let label = format!("{} {}({})", kind, name, param_list);

        Some(SignatureInformation {
            label: label.clone(),
            documentation: None,
            parameters: Some(
                params
                    .iter()
                    .map(|p| ParameterInformation {
                        label: ParameterLabel::Simple(p.clone()),
                        documentation: None,
                    })
                    .collect(),
            ),
            active_parameter: None,
        })
    }
}

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

fn find_similar_keyword(_word: &str) -> Option<&'static str> {
    None
}

fn close_block(
    stack: &mut Vec<(String, usize, Position)>,
    expected: &str,
    _diagnostics: &mut Vec<Diagnostic>,
    _offset: usize,
    _doc: &Document,
) -> bool {
    for i in (0..stack.len()).rev() {
        if stack[i].0 == expected {
            stack.remove(i);
            return true;
        }
    }
    false
}

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
        let doc = Document::new(r#"#DEFINE DATA_PATH "C:\Data\""#.to_string());
        let diagnostics = doc.diagnostics();

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
        let code = r#"
#DEFINE ASSERT(cond) IF .NOT. (cond) THEN MESSAGEBOX("Failed")
FUNCTION Test
ENDFUNCTION
"#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

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
        let code = r#"#DEFINE PATH_SEPARATOR "\""#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

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
        let code = "IF .T.\n    ? \"Hello\"\n* Missing ENDIF\n";
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

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
        let code = r#"
TEXT TO cOutput
Thank you for your order.
If you have questions, please contact us.
ENDTEXT
"#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

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
        let code = r#"
* This is for future use
* If we need to add more features
FUNCTION Test
ENDFUNC
"#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

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
        let code = r#"
LOCAL cMsg
cMsg = "This is for testing if strings work"
? "For each item in the list"
"#;
        let doc = Document::new(code.to_string());
        let diagnostics = doc.diagnostics();

        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("IF") || d.message.contains("FOR"))
            .collect();
        assert_eq!(errors.len(), 0, "Should not report keywords inside strings");
    }

    #[test]
    fn test_find_signature_with_lparameters() {
        let code = r#"
FUNCTION GetCustomerName(nCustomerId)
    LOCAL cName
    RETURN cName
ENDFUNC

PROCEDURE ProcessOrders
    LPARAMETERS dStartDate, dEndDate
    LOCAL nTotal
    nTotal = 0
ENDPROC
"#;
        let doc = Document::new(code.to_string());

        let sig1 = doc.find_signature("GetCustomerName");
        assert!(sig1.is_some(), "Should find GetCustomerName signature");
        let sig1 = sig1.unwrap();
        assert!(
            sig1.label.contains("GetCustomerName"),
            "Label should contain function name"
        );
        assert!(
            sig1.label.contains("nCustomerId"),
            "Label should contain parameter"
        );

        let sig2 = doc.find_signature("ProcessOrders");
        assert!(sig2.is_some(), "Should find ProcessOrders signature");
        let sig2 = sig2.unwrap();
        assert!(
            sig2.label.contains("ProcessOrders"),
            "Label should contain procedure name"
        );
        assert!(
            sig2.label.contains("dStartDate"),
            "Label should contain first parameter"
        );
        assert!(
            sig2.label.contains("dEndDate"),
            "Label should contain second parameter"
        );
    }

    #[test]
    fn test_find_signature_no_params() {
        let code = r#"
FUNCTION GetDate
    RETURN DATE()
ENDFUNC
"#;
        let doc = Document::new(code.to_string());
        let sig = doc.find_signature("GetDate");
        assert!(sig.is_some(), "Should find GetDate signature");
        let sig = sig.unwrap();
        assert!(
            sig.label.contains("GetDate()"),
            "Label should show empty parameter list"
        );
    }
}
