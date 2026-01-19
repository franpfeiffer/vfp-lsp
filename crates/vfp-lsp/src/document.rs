//! Document management for the LSP server.

use dashmap::DashMap;
use tower_lsp::lsp_types::*;
use vfp_lexer::{tokenize, Token, TokenKind};

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
                                    let name = &self.content
                                        [name_offset..name_offset + next.len as usize];
                                    let start = self.offset_to_position(start_offset);
                                    let end = self
                                        .offset_to_position(name_offset + next.len as usize);

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
                                                let name = &self.content
                                                    [name_offset..name_offset + name_token.len as usize];
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
