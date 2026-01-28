//! LSP backend implementation.

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use vfp_lexer::{TokenKind, is_keyword, is_sql_keyword};

use crate::capabilities::server_capabilities;
use crate::document::DocumentStore;
use crate::workspace::WorkspaceIndex;

pub struct Backend {
    client: Client,
    documents: DocumentStore,
    workspace: WorkspaceIndex,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DocumentStore::new(),
            workspace: WorkspaceIndex::new(),
        }
    }

    fn is_builtin_keyword(word: &str) -> bool {
        matches!(
            word,
            "FUNCTION"
                | "PROCEDURE"
                | "CLASS"
                | "DEFINE"
                | "ENDDEFINE"
                | "SELECT"
                | "FROM"
                | "WHERE"
                | "ORDER"
                | "BY"
                | "GROUP"
                | "HAVING"
                | "IF"
                | "ELSE"
                | "ENDIF"
                | "FOR"
                | "ENDFOR"
                | "WHILE"
                | "ENDWHILE"
                | "DO"
                | "CASE"
                | "OTHERWISE"
                | "ENDCASE"
                | "TRY"
                | "CATCH"
                | "FINALLY"
                | "ENDTRY"
                | "WITH"
                | "ENDWITH"
                | "RETURN"
                | "LOCAL"
                | "PRIVATE"
                | "PUBLIC"
                | "PARAMETERS"
        )
    }

    async fn publish_diagnostics(&self, uri: Url) {
        if let Some(doc) = self.documents.get(&uri) {
            let diagnostics = doc.diagnostics();
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    fn create_text_edit(uri: Url, range: Range, new_text: String) -> WorkspaceEdit {
        let mut changes = std::collections::HashMap::new();
        changes.insert(uri, vec![TextEdit { range, new_text }]);
        WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }
    }

    fn ranges_overlap(a: &Range, b: &Range) -> bool {
        if a.end.line < b.start.line {
            return false;
        }
        if a.end.line == b.start.line && a.end.character < b.start.character {
            return false;
        }
        if b.end.line < a.start.line {
            return false;
        }
        if b.end.line == a.start.line && b.end.character < a.start.character {
            return false;
        }
        true
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: server_capabilities(),
            server_info: Some(ServerInfo {
                name: "vfp-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        tracing::info!("VFP Language Server initialized");
        self.client
            .log_message(MessageType::INFO, "VFP Language Server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        self.documents.open(uri.clone(), content);

        if let Some(doc) = self.documents.get(&uri) {
            self.workspace.index_file(uri.clone(), &doc);
        }

        self.publish_diagnostics(uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().next() {
            self.documents.update(&uri, change.text);

            if let Some(doc) = self.documents.get(&uri) {
                self.workspace.remove_file(&uri);
                self.workspace.index_file(uri.clone(), &doc);
            }

            self.publish_diagnostics(uri).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(text) = params.text {
            self.documents.update(&uri, text);

            if let Some(doc) = self.documents.get(&uri) {
                self.workspace.remove_file(&uri);
                self.workspace.index_file(uri.clone(), &doc);
            }

            self.publish_diagnostics(uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.workspace.remove_file(&uri);
        self.documents.close(&uri);
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(word) = doc.word_at_position(position) else {
            return Ok(None);
        };

        let upper = word.to_ascii_uppercase();

        let content = if is_keyword(&upper) {
            get_keyword_documentation(&upper)
        } else if is_sql_keyword(&upper) {
            get_sql_keyword_documentation(&upper)
        } else {
            None
        };

        Ok(content.map(|c| Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: c,
            }),
            range: None,
        }))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let trigger = params
            .context
            .and_then(|c| c.trigger_character)
            .unwrap_or_default();

        let mut completions = Vec::new();

        if trigger == "#" {
            for directive in &[
                "DEFINE", "UNDEF", "IF", "IFDEF", "IFNDEF", "ELSE", "ELIF", "ENDIF", "INCLUDE",
            ] {
                completions.push(CompletionItem {
                    label: format!("#{}", directive),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("Preprocessor directive".to_string()),
                    ..Default::default()
                });
            }
        } else if trigger == "." {
            for item in &[".T.", ".F.", ".NULL.", ".AND.", ".OR.", ".NOT."] {
                completions.push(CompletionItem {
                    label: item.to_string(),
                    kind: Some(CompletionItemKind::CONSTANT),
                    ..Default::default()
                });
            }
        } else {
            for keyword in VFP_KEYWORDS {
                completions.push(CompletionItem {
                    label: keyword.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                });
            }

            for keyword in SQL_KEYWORDS {
                completions.push(CompletionItem {
                    label: keyword.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("SQL".to_string()),
                    ..Default::default()
                });
            }

            for func in BUILTIN_FUNCTIONS {
                completions.push(CompletionItem {
                    label: format!("{}()", func),
                    kind: Some(CompletionItemKind::FUNCTION),
                    insert_text: Some(format!("{}($0)", func)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }

            let mut seen = std::collections::HashSet::new();
            let mut pos = 0;
            for token in &doc.tokens {
                if token.kind == TokenKind::Ident {
                    let text = &doc.content[pos..pos + token.len as usize];
                    if !is_keyword(text) && !is_sql_keyword(text) && seen.insert(text.to_string()) {
                        completions.push(CompletionItem {
                            label: text.to_string(),
                            kind: Some(CompletionItemKind::VARIABLE),
                            ..Default::default()
                        });
                    }
                }
                pos += token.len as usize;
            }
        }

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let symbols = doc.symbols();
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(word) = doc.word_at_position(position) else {
            return Ok(None);
        };

        let upper_word = word.to_ascii_uppercase();
        if Self::is_builtin_keyword(&upper_word) {
            return Ok(None);
        }

        if let Some(range) = doc.find_definition(&word) {
            return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                uri.clone(),
                range,
            ))));
        }

        if let Some(symbols) = self.workspace.find_symbol(&word) {
            if let Some(first) = symbols.first() {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                    first.file_uri.clone(),
                    first.range,
                ))));
            }
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(word) = doc.word_at_position(position) else {
            return Ok(None);
        };

        let mut locations = Vec::new();

        for entry in self.documents.iter() {
            let (file_uri, file_doc) = entry.pair();
            let mut offset = 0;

            for token in &file_doc.tokens {
                if token.kind == TokenKind::Ident {
                    let text = &file_doc.content[offset..offset + token.len as usize];
                    if text.eq_ignore_ascii_case(&word) {
                        let start = file_doc.offset_to_position(offset);
                        let end = file_doc.offset_to_position(offset + token.len as usize);
                        locations.push(Location::new(file_uri.clone(), Range::new(start, end)));
                    }
                }
                offset += token.len as usize;
            }
        }

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let mut data = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_col = 0u32;
        let mut offset = 0;

        for token in &doc.tokens {
            let token_type = match token.kind {
                TokenKind::CommentStar | TokenKind::CommentAmpAmp | TokenKind::CommentNote => {
                    Some(17)
                }
                TokenKind::Literal { kind } => match kind {
                    vfp_lexer::LiteralKind::StringDouble { .. }
                    | vfp_lexer::LiteralKind::StringSingle { .. }
                    | vfp_lexer::LiteralKind::StringBracket { .. } => Some(18),
                    vfp_lexer::LiteralKind::Int
                    | vfp_lexer::LiteralKind::Float
                    | vfp_lexer::LiteralKind::Hex
                    | vfp_lexer::LiteralKind::Date
                    | vfp_lexer::LiteralKind::DateTime => Some(19),
                },
                TokenKind::True | TokenKind::False | TokenKind::Null => Some(19),
                TokenKind::PreDefine
                | TokenKind::PreUndef
                | TokenKind::PreIf
                | TokenKind::PreIfDef
                | TokenKind::PreIfNDef
                | TokenKind::PreElse
                | TokenKind::PreElif
                | TokenKind::PreEndIf
                | TokenKind::PreInclude => Some(14),
                TokenKind::DotAnd | TokenKind::DotOr | TokenKind::DotNot => Some(21),
                TokenKind::Ident => {
                    let text = &doc.content[offset..offset + token.len as usize];
                    if is_keyword(text) || is_sql_keyword(text) {
                        Some(15)
                    } else {
                        None
                    }
                }
                _ => None,
            };

            if let Some(token_type) = token_type {
                let pos = doc.offset_to_position(offset);
                let delta_line = pos.line - prev_line;
                let delta_col = if delta_line == 0 {
                    pos.character - prev_col
                } else {
                    pos.character
                };

                data.push(SemanticToken {
                    delta_line,
                    delta_start: delta_col,
                    length: token.len,
                    token_type,
                    token_modifiers_bitset: 0,
                });

                prev_line = pos.line;
                prev_col = pos.character;
            }

            offset += token.len as usize;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = doc.position_to_offset(position);
        let Some(function_name) = find_function_name_at_position(&doc, offset) else {
            return Ok(None);
        };

        let Some(signature_info) = doc.find_signature(&function_name) else {
            return Ok(None);
        };

        Ok(Some(SignatureHelp {
            signatures: vec![signature_info],
            active_signature: Some(0),
            active_parameter: None,
        }))
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let results = self.workspace.workspace_symbols(&params.query);
        if results.is_empty() {
            Ok(None)
        } else {
            Ok(Some(results))
        }
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(word) = doc.word_at_position(position) else {
            return Ok(None);
        };

        let mut highlights = Vec::new();
        let mut offset = 0;

        for token in &doc.tokens {
            if token.kind == TokenKind::Ident {
                let text = &doc.content[offset..offset + token.len as usize];
                if text.eq_ignore_ascii_case(&word) {
                    let start = doc.offset_to_position(offset);
                    let end = doc.offset_to_position(offset + token.len as usize);
                    highlights.push(DocumentHighlight {
                        range: Range::new(start, end),
                        kind: Some(DocumentHighlightKind::TEXT),
                    });
                }
            }
            offset += token.len as usize;
        }

        if highlights.is_empty() {
            Ok(None)
        } else {
            Ok(Some(highlights))
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = &params.new_name;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(word) = doc.word_at_position(position) else {
            return Ok(None);
        };

        let mut changes = std::collections::HashMap::new();

        for entry in self.documents.iter() {
            let (file_uri, file_doc) = entry.pair();
            let mut file_edits = Vec::new();
            let mut offset = 0;

            for token in &file_doc.tokens {
                if token.kind == TokenKind::Ident {
                    let text = &file_doc.content[offset..offset + token.len as usize];
                    if text.eq_ignore_ascii_case(&word) {
                        let start = file_doc.offset_to_position(offset);
                        let end = file_doc.offset_to_position(offset + token.len as usize);
                        file_edits.push(TextEdit {
                            range: Range::new(start, end),
                            new_text: new_name.clone(),
                        });
                    }
                }
                offset += token.len as usize;
            }

            if !file_edits.is_empty() {
                changes.insert(file_uri.clone(), file_edits);
            }
        }

        if changes.is_empty() {
            Ok(None)
        } else {
            Ok(Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }))
        }
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = &params.text_document.uri;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let mut ranges = Vec::new();
        let mut block_stack: Vec<(String, u32)> = Vec::new();
        let mut offset = 0;

        for token in &doc.tokens {
            if token.kind == TokenKind::Ident {
                let text = &doc.content[offset..offset + token.len as usize];
                let upper = text.to_ascii_uppercase();
                let pos = doc.offset_to_position(offset);

                match upper.as_str() {
                    "FUNCTION" | "PROCEDURE" | "DEFINE" => {
                        block_stack.push((upper, pos.line));
                    }
                    "IF" => {
                        block_stack.push(("IF".to_string(), pos.line));
                    }
                    "FOR" | "WHILE" | "SCAN" | "TRY" | "WITH" | "TEXT" => {
                        block_stack.push((upper, pos.line));
                    }
                    "DO" => {
                        block_stack.push(("DO".to_string(), pos.line));
                    }
                    "CASE" => {
                        if let Some((kind, _)) = block_stack.last_mut() {
                            if kind == "DO" {
                                *kind = "DO_CASE".to_string();
                            }
                        }
                    }
                    "ENDFUNC" | "ENDPROC" | "ENDDEFINE" => {
                        if let Some((kind, start_line)) = block_stack.pop() {
                            if matches!(kind.as_str(), "FUNCTION" | "PROCEDURE" | "DEFINE") {
                                ranges.push(FoldingRange {
                                    start_line,
                                    start_character: None,
                                    end_line: pos.line,
                                    end_character: None,
                                    kind: Some(FoldingRangeKind::Region),
                                    collapsed_text: None,
                                });
                            }
                        }
                    }
                    "ENDIF" => {
                        if let Some((kind, start_line)) = block_stack.pop() {
                            if kind == "IF" {
                                ranges.push(FoldingRange {
                                    start_line,
                                    start_character: None,
                                    end_line: pos.line,
                                    end_character: None,
                                    kind: Some(FoldingRangeKind::Region),
                                    collapsed_text: None,
                                });
                            }
                        }
                    }
                    "ENDFOR" | "ENDWHILE" | "ENDSCAN" | "ENDTRY" | "ENDWITH" | "ENDTEXT" => {
                        if let Some((_, start_line)) = block_stack.pop() {
                            ranges.push(FoldingRange {
                                start_line,
                                start_character: None,
                                end_line: pos.line,
                                end_character: None,
                                kind: Some(FoldingRangeKind::Region),
                                collapsed_text: None,
                            });
                        }
                    }
                    "ENDDO" | "ENDCASE" => {
                        if let Some((_, start_line)) = block_stack.pop() {
                            ranges.push(FoldingRange {
                                start_line,
                                start_character: None,
                                end_line: pos.line,
                                end_character: None,
                                kind: Some(FoldingRangeKind::Region),
                                collapsed_text: None,
                            });
                        }
                    }
                    _ => {}
                }
            }
            offset += token.len as usize;
        }

        if ranges.is_empty() {
            Ok(None)
        } else {
            Ok(Some(ranges))
        }
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let diagnostics = doc.diagnostics();
        let mut actions = Vec::new();

        for diagnostic in diagnostics {
            if !Self::ranges_overlap(&diagnostic.range, &params.range) {
                continue;
            }

            if let Some(NumberOrString::String(code)) = &diagnostic.code {
                let action = match code.as_str() {
                    "unterminated_string" => self.fix_unterminated_string(uri, &diagnostic),
                    "misspelled_keyword" => self.fix_misspelled_keyword(uri, &diagnostic),
                    "unclosed_block" => self.fix_unclosed_block(uri, &diagnostic),
                    "orphaned_end" => Some(self.fix_orphaned_end(uri, &diagnostic)),
                    _ => None,
                };

                if let Some(action) = action {
                    actions.push(CodeActionOrCommand::CodeAction(action));
                }
            }
        }

        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }
}

impl Backend {
    fn fix_unterminated_string(&self, uri: &Url, diagnostic: &Diagnostic) -> Option<CodeAction> {
        let doc = self.documents.get(uri)?;
        let offset = doc.position_to_offset(diagnostic.range.start);
        let (token, _) = doc.token_at_offset(offset)?;

        let (closing_char, description) = match token.kind {
            TokenKind::Literal {
                kind: vfp_lexer::LiteralKind::StringDouble { terminated: false },
            } => ("\"", "double quote"),
            TokenKind::Literal {
                kind: vfp_lexer::LiteralKind::StringSingle { terminated: false },
            } => ("'", "single quote"),
            TokenKind::Literal {
                kind: vfp_lexer::LiteralKind::StringBracket { terminated: false },
            } => ("]", "bracket"),
            _ => return None,
        };

        let edit = Self::create_text_edit(
            uri.clone(),
            Range::new(diagnostic.range.end, diagnostic.range.end),
            closing_char.to_string(),
        );

        Some(CodeAction {
            title: format!("Add closing {}", description),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: Some(edit),
            is_preferred: Some(true),
            command: None,
            disabled: None,
            data: None,
        })
    }

    fn fix_misspelled_keyword(&self, uri: &Url, diagnostic: &Diagnostic) -> Option<CodeAction> {
        let suggestion = extract_suggestion(&diagnostic.message)?;

        let edit = Self::create_text_edit(uri.clone(), diagnostic.range, suggestion.clone());

        Some(CodeAction {
            title: format!("Change to '{}'", suggestion),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: Some(edit),
            is_preferred: Some(true),
            command: None,
            disabled: None,
            data: None,
        })
    }

    fn fix_unclosed_block(&self, uri: &Url, diagnostic: &Diagnostic) -> Option<CodeAction> {
        let end_keyword = extract_expected(&diagnostic.message)?;
        let doc = self.documents.get(uri)?;

        let lines = doc.content.lines().count() as u32;
        let last_line_len = doc.content.lines().last().map(|l| l.len()).unwrap_or(0) as u32;
        let insert_pos = Position::new(lines.saturating_sub(1), last_line_len);

        let edit = Self::create_text_edit(
            uri.clone(),
            Range::new(insert_pos, insert_pos),
            format!("\n{}", end_keyword),
        );

        Some(CodeAction {
            title: format!("Add {}", end_keyword),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: Some(edit),
            is_preferred: Some(true),
            command: None,
            disabled: None,
            data: None,
        })
    }

    fn fix_orphaned_end(&self, uri: &Url, diagnostic: &Diagnostic) -> CodeAction {
        let edit = Self::create_text_edit(uri.clone(), diagnostic.range, String::new());

        CodeAction {
            title: "Remove orphaned statement".to_string(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: Some(edit),
            is_preferred: Some(true),
            command: None,
            disabled: None,
            data: None,
        }
    }
}

fn extract_suggestion(msg: &str) -> Option<String> {
    let start = msg.find('\'')?;
    let end = msg[start + 1..].find('\'')?;
    Some(msg[start + 1..start + 1 + end].to_string())
}

fn extract_expected(msg: &str) -> Option<String> {
    let pos = msg.find("Expected ")?;
    let rest = &msg[pos + 9..];
    let keyword = rest.split(" or").next()?.trim();
    Some(keyword.to_string())
}

fn find_function_name_at_position(
    doc: &crate::document::Document,
    offset: usize,
) -> Option<String> {
    let mut current_offset = offset;
    let mut paren_count = 0;

    while current_offset > 0 {
        current_offset -= 1;
        let ch = doc.content.chars().nth(current_offset)?;

        if ch == ')' {
            paren_count += 1;
        } else if ch == '(' {
            if paren_count == 0 {
                let mut name_end = current_offset;
                while name_end > 0 && doc.content.chars().nth(name_end - 1)?.is_whitespace() {
                    name_end -= 1;
                }

                let mut name_start = name_end;
                while name_start > 0 {
                    let ch = doc.content.chars().nth(name_start - 1)?;
                    if ch.is_alphanumeric() || ch == '_' {
                        name_start -= 1;
                    } else {
                        break;
                    }
                }

                if name_start < name_end {
                    return Some(doc.content[name_start..name_end].to_string());
                }
                return None;
            }
            paren_count -= 1;
        }
    }

    None
}

fn get_keyword_documentation(keyword: &str) -> Option<String> {
    Some(match keyword {
        "FUNCTION" => "**FUNCTION**\n\nDefines a user-defined function.\n\n```foxpro\nFUNCTION name(parameters)\n    * code\n    RETURN value\nENDFUNC\n```".to_string(),
        "ENDFUNC" => "**ENDFUNC**\n\nEnds a FUNCTION definition.\n\n```foxpro\nFUNCTION GetTotal\n    RETURN nTotal\nENDFUNC\n```".to_string(),
        "PROCEDURE" => "**PROCEDURE**\n\nDefines a procedure (subroutine).\n\n```foxpro\nPROCEDURE name\n    LPARAMETERS param1, param2\n    * code\nENDPROC\n```".to_string(),
        "ENDPROC" => "**ENDPROC**\n\nEnds a PROCEDURE definition.\n\n```foxpro\nPROCEDURE Init\n    * initialization code\nENDPROC\n```".to_string(),
        "IF" => "**IF**\n\nConditional execution.\n\n```foxpro\nIF condition\n    * code\nELSE\n    * code\nENDIF\n```".to_string(),
        "ELSE" => "**ELSE**\n\nAlternative branch in IF statement.\n\n```foxpro\nIF condition\n    * true branch\nELSE\n    * false branch\nENDIF\n```".to_string(),
        "ELSEIF" => "**ELSEIF**\n\nAdditional condition in IF statement.\n\n```foxpro\nIF condition1\n    * code\nELSEIF condition2\n    * code\nENDIF\n```".to_string(),
        "ENDIF" => "**ENDIF**\n\nEnds an IF statement.\n\n```foxpro\nIF .T.\n    ? \"True\"\nENDIF\n```".to_string(),
        "DO" => "**DO**\n\nExecute a program, procedure, or loop.\n\n```foxpro\nDO WHILE condition\n    * code\nENDDO\n\nDO CASE\n    CASE condition\n        * code\nENDCASE\n\nDO MyProcedure\n```".to_string(),
        "WHILE" => "**WHILE**\n\nLoop while condition is true.\n\n```foxpro\nDO WHILE NOT EOF()\n    * process record\n    SKIP\nENDDO\n```".to_string(),
        "ENDDO" => "**ENDDO**\n\nEnds a DO WHILE or DO CASE loop.\n\n```foxpro\nDO WHILE .T.\n    IF condition\n        EXIT\n    ENDIF\nENDDO\n```".to_string(),
        "FOR" => "**FOR**\n\nLoop through a range of values.\n\n```foxpro\nFOR i = 1 TO 10 STEP 1\n    * code\nENDFOR\n\nFOR EACH item IN collection\n    * code\nENDFOR\n```".to_string(),
        "EACH" => "**EACH**\n\nIterates through collection items.\n\n```foxpro\nFOR EACH oControl IN thisform.Controls\n    oControl.Enabled = .T.\nENDFOR\n```".to_string(),
        "ENDFOR" => "**ENDFOR**\n\nEnds a FOR loop.\n\n```foxpro\nFOR i = 1 TO 10\n    ? i\nENDFOR\n```".to_string(),
        "NEXT" => "**NEXT**\n\nAlternative to ENDFOR (ends a FOR loop).\n\n```foxpro\nFOR i = 1 TO 10\n    ? i\nNEXT\n```".to_string(),
        "TO" => "**TO**\n\nSpecifies the end value in a FOR loop.\n\n```foxpro\nFOR i = 1 TO 100\n    * code\nNEXT\n```".to_string(),
        "STEP" => "**STEP**\n\nSpecifies increment in FOR loop.\n\n```foxpro\nFOR i = 0 TO 100 STEP 10\n    ? i\nNEXT\n```".to_string(),
        "IN" => "**IN**\n\nSpecifies collection in FOR EACH or work area.\n\n```foxpro\nFOR EACH item IN aArray\nENDFOR\n\nUSE customers IN 0\n```".to_string(),
        "CASE" => "**CASE**\n\nDefines a condition branch in DO CASE.\n\n```foxpro\nDO CASE\n    CASE condition1\n        * code\n    CASE condition2\n        * code\nENDCASE\n```".to_string(),
        "OTHERWISE" => "**OTHERWISE**\n\nDefault case in DO CASE statement.\n\n```foxpro\nDO CASE\n    CASE x = 1\n        * code\n    OTHERWISE\n        * default code\nENDCASE\n```".to_string(),
        "ENDCASE" => "**ENDCASE**\n\nEnds a DO CASE statement.\n\n```foxpro\nDO CASE\n    CASE condition\n        * code\nENDCASE\n```".to_string(),
        "SCAN" => "**SCAN**\n\nLoop through records in a table.\n\n```foxpro\nSCAN FOR active = .T.\n    * process each record\nENDSCAN\n```".to_string(),
        "ENDSCAN" => "**ENDSCAN**\n\nEnds a SCAN loop.\n\n```foxpro\nSCAN\n    IF condition\n        LOOP\n    ENDIF\nENDSCAN\n```".to_string(),
        "EXIT" => "**EXIT**\n\nExits from a loop.\n\n```foxpro\nDO WHILE .T.\n    IF condition\n        EXIT\n    ENDIF\nENDDO\n```".to_string(),
        "LOOP" => "**LOOP**\n\nSkips to next iteration of a loop.\n\n```foxpro\nSCAN\n    IF deleted()\n        LOOP\n    ENDIF\n    * process record\nENDSCAN\n```".to_string(),
        "TRY" => "**TRY**\n\nStructured error handling.\n\n```foxpro\nTRY\n    * code that might fail\nCATCH TO oException\n    * handle error\nFINALLY\n    * cleanup\nENDTRY\n```".to_string(),
        "CATCH" => "**CATCH**\n\nHandles exceptions in TRY block.\n\n```foxpro\nTRY\n    * risky code\nCATCH TO oError\n    MESSAGEBOX(oError.Message)\nENDTRY\n```".to_string(),
        "FINALLY" => "**FINALLY**\n\nCode that always executes in TRY.\n\n```foxpro\nTRY\n    * code\nCATCH\n    * error handling\nFINALLY\n    * cleanup (always runs)\nENDTRY\n```".to_string(),
        "ENDTRY" => "**ENDTRY**\n\nEnds a TRY block.\n\n```foxpro\nTRY\n    * code\nCATCH\n    * error handling\nENDTRY\n```".to_string(),
        "THROW" => "**THROW**\n\nThrows an exception.\n\n```foxpro\nIF nValue < 0\n    THROW \"Invalid value\"\nENDIF\n```".to_string(),
        "LOCAL" => "**LOCAL**\n\nDeclare local variables (scoped to current procedure).\n\n```foxpro\nLOCAL cName, nValue, aArray[10]\nLOCAL oObject AS MyClass\n```".to_string(),
        "PRIVATE" => "**PRIVATE**\n\nDeclare private variables (hidden from higher-level routines).\n\n```foxpro\nPRIVATE cName, nValue\n```".to_string(),
        "PUBLIC" => "**PUBLIC**\n\nDeclare public variables (accessible everywhere).\n\n```foxpro\nPUBLIC gAppName, gUserId, gaConfig[10]\n```".to_string(),
        "DIMENSION" => "**DIMENSION**\n\nDefine or redimension an array.\n\n```foxpro\nDIMENSION aData[100, 5]\nDIMENSION aList[ALEN(aList) + 1]\n```".to_string(),
        "DECLARE" => "**DECLARE**\n\nDeclare external API functions.\n\n```foxpro\nDECLARE INTEGER MessageBox IN user32 ;\n    INTEGER hWnd, STRING lpText, ;\n    STRING lpCaption, INTEGER uType\n```".to_string(),
        "EXTERNAL" => "**EXTERNAL**\n\nUsed with DECLARE for external libraries.\n\n```foxpro\nDECLARE EXTERNAL MyDll MyFunction\n```".to_string(),
        "DEFINE" => "**DEFINE CLASS**\n\nDefine a new class.\n\n```foxpro\nDEFINE CLASS MyClass AS ParentClass\n    Property = value\n    \n    PROCEDURE Method\n        * code\n    ENDPROC\nENDDEFINE\n```".to_string(),
        "CLASS" => "**CLASS**\n\nUsed with DEFINE to create a class.\n\n```foxpro\nDEFINE CLASS Customer AS Custom\n    Name = \"\"\n    Email = \"\"\nENDDEFINE\n```".to_string(),
        "ENDDEFINE" => "**ENDDEFINE**\n\nEnds a class definition.\n\n```foxpro\nDEFINE CLASS MyClass AS Custom\n    * properties and methods\nENDDEFINE\n```".to_string(),
        "AS" => "**AS**\n\nSpecifies parent class or data type.\n\n```foxpro\nDEFINE CLASS MyClass AS Custom\nLOCAL oObject AS MyClass\n```".to_string(),
        "OF" => "**OF**\n\nSpecifies container or library.\n\n```foxpro\nCREATEOBJECT(\"form\") OF MyLibrary\n```".to_string(),
        "OLEPUBLIC" => "**OLEPUBLIC**\n\nMakes class available to COM/OLE.\n\n```foxpro\nDEFINE CLASS MyServer AS Custom OLEPUBLIC\n    * automation server\nENDDEFINE\n```".to_string(),
        "THIS" => "**THIS**\n\nReference to the current object instance.\n\n```foxpro\nTHIS.Caption = \"New Title\"\nTHIS.Refresh()\n```".to_string(),
        "THISFORM" => "**THISFORM**\n\nReference to the form containing the current object.\n\n```foxpro\nTHISFORM.Caption = \"Title\"\nTHISFORM.Release()\n```".to_string(),
        "THISFORMSET" => "**THISFORMSET**\n\nReference to the formset containing the current object.\n\n```foxpro\nTHISFORMSET.Release()\n```".to_string(),
        "DODEFAULT" => "**DODEFAULT**\n\nCalls parent class method.\n\n```foxpro\nPROCEDURE Click\n    DODEFAULT()  && call parent Click\n    * additional code\nENDPROC\n```".to_string(),
        "NODEFAULT" => "**NODEFAULT**\n\nPrevents default behavior.\n\n```foxpro\nPROCEDURE KeyPress\n    LPARAMETERS nKeyCode, nShiftAltCtrl\n    NODEFAULT  && prevent default key handling\nENDPROC\n```".to_string(),
        "PROTECTED" => "**PROTECTED**\n\nMakes member accessible only within class and subclasses.\n\n```foxpro\nPROTECTED PROCEDURE Init\n    * code\nENDPROC\n```".to_string(),
        "HIDDEN" => "**HIDDEN**\n\nHides member from outside access.\n\n```foxpro\nHIDDEN nInternalValue\n```".to_string(),
        "WITH" => "**WITH**\n\nSet object context for property access.\n\n```foxpro\nWITH thisform.grid1\n    .RecordSource = \"customers\"\n    .Refresh()\nENDWITH\n```".to_string(),
        "ENDWITH" => "**ENDWITH**\n\nEnds a WITH block.\n\n```foxpro\nWITH oObject\n    .Property = value\nENDWITH\n```".to_string(),
        "TEXT" => "**TEXT**\n\nDefines a block of literal text.\n\n```foxpro\nTEXT TO cSQL TEXTMERGE NOSHOW\n    SELECT * FROM customers\n    WHERE id = <<nId>>\nENDTEXT\n```".to_string(),
        "ENDTEXT" => "**ENDTEXT**\n\nEnds a TEXT block.\n\n```foxpro\nTEXT TO cHTML\n    <html>...</html>\nENDTEXT\n```".to_string(),
        "LPARAMETERS" => "**LPARAMETERS**\n\nDefines local parameters in a procedure/function.\n\n```foxpro\nPROCEDURE MyProc\n    LPARAMETERS cName, nAge, lActive\n    * code\nENDPROC\n```".to_string(),
        "PARAMETERS" => "**PARAMETERS**\n\nDefines private parameters (older style).\n\n```foxpro\nPROCEDURE OldStyle\n    PARAMETERS p1, p2, p3\n    * code\nENDPROC\n```".to_string(),
        "RETURN" => "**RETURN**\n\nReturn from a function or procedure.\n\n```foxpro\nFUNCTION GetTotal\n    RETURN nSubtotal + nTax\nENDFUNC\n```".to_string(),
        "USE" => "**USE**\n\nOpens a table in a work area.\n\n```foxpro\nUSE customers\nUSE orders IN 0 SHARED\nUSE  && close table in current work area\n```".to_string(),
        "ALIAS" => "**ALIAS**\n\nSpecifies or returns table alias.\n\n```foxpro\nUSE customers ALIAS cust\n? ALIAS()  && returns current alias\n```".to_string(),
        "EXCLUSIVE" => "**EXCLUSIVE**\n\nOpens table for exclusive access.\n\n```foxpro\nUSE customers EXCLUSIVE\n```".to_string(),
        "SHARED" => "**SHARED**\n\nOpens table for shared access.\n\n```foxpro\nUSE customers SHARED\n```".to_string(),
        "NOUPDATE" => "**NOUPDATE**\n\nOpens table as read-only.\n\n```foxpro\nUSE customers NOUPDATE\n```".to_string(),
        "INDEX" => "**INDEX**\n\nCreates or manages table indexes.\n\n```foxpro\nINDEX ON lastname TAG last\nUSE customers INDEX custname\n```".to_string(),
        "TAG" => "**TAG**\n\nSpecifies index tag name.\n\n```foxpro\nINDEX ON lastname TAG last\nSET ORDER TO TAG last\n```".to_string(),
        "DESCENDING" => "**DESCENDING**\n\nCreates descending index.\n\n```foxpro\nINDEX ON orderdate TAG recent DESCENDING\n```".to_string(),
        "ASCENDING" => "**ASCENDING**\n\nCreates ascending index (default).\n\n```foxpro\nINDEX ON name TAG nameidx ASCENDING\n```".to_string(),
        "DATABASE" => "**DATABASE**\n\nRefers to a VFP database container.\n\n```foxpro\nOPEN DATABASE mydata\nUSE customers DATABASE mydata\n```".to_string(),
        "DATABASES" => "**DATABASES**\n\nRefers to multiple database containers.\n\n```foxpro\nCLOSE DATABASES ALL\n```".to_string(),
        "TABLES" => "**TABLES**\n\nRefers to multiple tables.\n\n```foxpro\nCLOSE TABLES ALL\n```".to_string(),
        "INDEXES" => "**INDEXES**\n\nRefers to multiple indexes.\n\n```foxpro\nCLOSE INDEXES\n```".to_string(),
        "OPEN" => "**OPEN**\n\nOpens a database or file.\n\n```foxpro\nOPEN DATABASE mydata\n```".to_string(),
        "CLOSE" => "**CLOSE**\n\nCloses databases, tables, or files.\n\n```foxpro\nCLOSE DATABASES\nCLOSE TABLES ALL\n```".to_string(),
        "CLEAR" => "**CLEAR**\n\nClears memory variables or screen.\n\n```foxpro\nCLEAR\nCLEAR ALL\nCLEAR MEMORY\n```".to_string(),
        "STORE" => "**STORE**\n\nAssigns value to multiple variables.\n\n```foxpro\nSTORE 0 TO nCount, nTotal, nSum\nSTORE .F. TO lActive, lVisible\n```".to_string(),
        "RELEASE" => "**RELEASE**\n\nReleases variables or objects from memory.\n\n```foxpro\nRELEASE nTemp, cBuffer\nRELEASE ALL\n```".to_string(),
        "NOVALIDATE" => "**NOVALIDATE**\n\nSkips validation rules.\n\n```foxpro\nREPLACE field WITH value NOVALIDATE\n```".to_string(),
        "THEN" => "**THEN**\n\nOptional keyword in IF statements.\n\n```foxpro\nIF condition THEN\n    * code\nENDIF\n```".to_string(),
        _ => return None,
    })
}

fn get_sql_keyword_documentation(keyword: &str) -> Option<String> {
    Some(match keyword {
        "SELECT" => "**SELECT**\n\nRetrieve data from tables.\n\n```foxpro\nSELECT columns FROM table ;\n    WHERE condition ;\n    ORDER BY column ;\n    INTO CURSOR cursorname\n```".to_string(),
        "FROM" => "**FROM**\n\nSpecifies the table(s) to query.\n\n```foxpro\nSELECT * FROM customers\nSELECT * FROM orders, items\n```".to_string(),
        "WHERE" => "**WHERE**\n\nFilters records based on a condition.\n\n```foxpro\nSELECT * FROM customers ;\n    WHERE city = 'New York'\n```".to_string(),
        "INTO" => "**INTO**\n\nSpecifies the destination for query results.\n\n```foxpro\nSELECT * FROM customers ;\n    INTO CURSOR curTemp\n\nSELECT * FROM orders ;\n    INTO ARRAY aResults\n```".to_string(),
        "CURSOR" => "**CURSOR**\n\nA temporary result set from a query.\n\n```foxpro\nSELECT * FROM customers ;\n    WHERE active = .T. ;\n    INTO CURSOR curActive\n```".to_string(),
        "TABLE" => "**TABLE**\n\nRefers to a database table.\n\n```foxpro\nSELECT * FROM customers ;\n    INTO TABLE backup\n```".to_string(),
        "ORDER" => "**ORDER BY**\n\nSorts query results.\n\n```foxpro\nSELECT * FROM customers ;\n    ORDER BY lastname, firstname\n```".to_string(),
        "BY" => "**BY**\n\nUsed with ORDER BY and GROUP BY.\n\n```foxpro\nORDER BY column\nGROUP BY category\n```".to_string(),
        "GROUP" => "**GROUP BY**\n\nGroups rows with the same values.\n\n```foxpro\nSELECT category, COUNT(*) ;\n    FROM products ;\n    GROUP BY category\n```".to_string(),
        "HAVING" => "**HAVING**\n\nFilters grouped results.\n\n```foxpro\nSELECT category, COUNT(*) as cnt ;\n    FROM products ;\n    GROUP BY category ;\n    HAVING cnt > 10\n```".to_string(),
        "INSERT" => "**INSERT**\n\nInsert new records.\n\n```foxpro\nINSERT INTO table (col1, col2) ;\n    VALUES (val1, val2)\n```".to_string(),
        "UPDATE" => "**UPDATE**\n\nUpdate existing records.\n\n```foxpro\nUPDATE table ;\n    SET column = value ;\n    WHERE condition\n```".to_string(),
        "DELETE" => "**DELETE**\n\nDelete records from a table.\n\n```foxpro\nDELETE FROM table WHERE condition\n```".to_string(),
        "SET" => "**SET**\n\nAssigns values in UPDATE statements or sets environment options.\n\n```foxpro\nUPDATE customers ;\n    SET status = 'Active' ;\n    WHERE lastorder > DATE()-30\n```".to_string(),
        "VALUES" => "**VALUES**\n\nSpecifies values for INSERT.\n\n```foxpro\nINSERT INTO customers (name, city) ;\n    VALUES ('John Doe', 'Boston')\n```".to_string(),
        "JOIN" => "**JOIN**\n\nCombine rows from multiple tables.\n\n```foxpro\nSELECT * FROM table1 ;\n    INNER JOIN table2 ON table1.id = table2.id\n```".to_string(),
        "INNER" => "**INNER JOIN**\n\nReturns matching rows from both tables.\n\n```foxpro\nSELECT * FROM orders ;\n    INNER JOIN customers ;\n    ON orders.custid = customers.id\n```".to_string(),
        "LEFT" => "**LEFT JOIN**\n\nReturns all rows from left table and matching rows from right.\n\n```foxpro\nSELECT * FROM customers ;\n    LEFT JOIN orders ;\n    ON customers.id = orders.custid\n```".to_string(),
        "RIGHT" => "**RIGHT JOIN**\n\nReturns all rows from right table and matching rows from left.\n\n```foxpro\nSELECT * FROM orders ;\n    RIGHT JOIN customers ;\n    ON orders.custid = customers.id\n```".to_string(),
        "OUTER" => "**OUTER JOIN**\n\nReturns all rows when there's a match in either table.\n\n```foxpro\nSELECT * FROM table1 ;\n    FULL OUTER JOIN table2 ;\n    ON table1.id = table2.id\n```".to_string(),
        "FULL" => "**FULL OUTER JOIN**\n\nReturns all rows from both tables.\n\n```foxpro\nSELECT * FROM table1 ;\n    FULL OUTER JOIN table2 ;\n    ON table1.id = table2.id\n```".to_string(),
        "CROSS" => "**CROSS JOIN**\n\nReturns the Cartesian product of both tables.\n\n```foxpro\nSELECT * FROM colors ;\n    CROSS JOIN sizes\n```".to_string(),
        "ON" => "**ON**\n\nSpecifies join condition.\n\n```foxpro\nSELECT * FROM orders ;\n    JOIN customers ;\n    ON orders.custid = customers.id\n```".to_string(),
        "DISTINCT" => "**DISTINCT**\n\nReturns only unique rows.\n\n```foxpro\nSELECT DISTINCT city FROM customers\n```".to_string(),
        "TOP" => "**TOP**\n\nLimits the number of rows returned.\n\n```foxpro\nSELECT TOP 10 * FROM customers ;\n    ORDER BY sales DESC\n```".to_string(),
        "UNION" => "**UNION**\n\nCombines results from multiple SELECT statements.\n\n```foxpro\nSELECT name FROM customers ;\n    UNION ;\n    SELECT name FROM suppliers\n```".to_string(),
        "ALL" => "**ALL**\n\nIncludes all rows including duplicates.\n\n```foxpro\nSELECT ALL city FROM customers\nUNION ALL\n```".to_string(),
        "AND" => "**AND**\n\nLogical AND operator.\n\n```foxpro\nWHERE active = .T. AND balance > 0\n```".to_string(),
        "OR" => "**OR**\n\nLogical OR operator.\n\n```foxpro\nWHERE city = 'Boston' OR city = 'NYC'\n```".to_string(),
        "NOT" => "**NOT**\n\nLogical NOT operator.\n\n```foxpro\nWHERE NOT deleted\n```".to_string(),
        "LIKE" => "**LIKE**\n\nPattern matching with wildcards.\n\n```foxpro\nWHERE name LIKE 'John%'\nWHERE email LIKE '%@example.com'\n```".to_string(),
        "BETWEEN" => "**BETWEEN**\n\nChecks if value is within a range.\n\n```foxpro\nWHERE orderdate BETWEEN DATE()-30 AND DATE()\nWHERE price BETWEEN 10 AND 100\n```".to_string(),
        "IS" => "**IS**\n\nCompares with NULL values.\n\n```foxpro\nWHERE email IS NULL\nWHERE status IS NOT NULL\n```".to_string(),
        "NULL" => "**NULL**\n\nRepresents missing or undefined value.\n\n```foxpro\nWHERE field IS NULL\nWHERE field IS NOT NULL\n```".to_string(),
        "CREATE" => "**CREATE**\n\nCreates database objects.\n\n```foxpro\nCREATE TABLE customers (id I, name C(50))\nCREATE CURSOR temp (field1 C(10))\n```".to_string(),
        "ALTER" => "**ALTER TABLE**\n\nModifies table structure.\n\n```foxpro\nALTER TABLE customers ;\n    ADD COLUMN email C(100)\n```".to_string(),
        "DROP" => "**DROP**\n\nRemoves database objects.\n\n```foxpro\nDROP TABLE olddata\n```".to_string(),
        "ADD" => "**ADD**\n\nAdds columns or constraints.\n\n```foxpro\nALTER TABLE customers ;\n    ADD COLUMN phone C(20)\n```".to_string(),
        "COLUMN" => "**COLUMN**\n\nRefers to a table column.\n\n```foxpro\nALTER TABLE customers ;\n    ADD COLUMN email C(100)\n```".to_string(),
        "ASC" => "**ASC**\n\nAscending sort order (default).\n\n```foxpro\nORDER BY lastname ASC\n```".to_string(),
        "DESC" => "**DESC**\n\nDescending sort order.\n\n```foxpro\nORDER BY orderdate DESC\n```".to_string(),
        "COUNT" => "**COUNT()**\n\nCounts rows or non-null values.\n\n```foxpro\nSELECT COUNT(*) FROM customers\nSELECT COUNT(email) FROM customers\n```".to_string(),
        "SUM" => "**SUM()**\n\nCalculates sum of values.\n\n```foxpro\nSELECT SUM(amount) FROM orders\n```".to_string(),
        "AVG" => "**AVG()**\n\nCalculates average of values.\n\n```foxpro\nSELECT AVG(price) FROM products\n```".to_string(),
        "MIN" => "**MIN()**\n\nFinds minimum value.\n\n```foxpro\nSELECT MIN(price) FROM products\n```".to_string(),
        "MAX" => "**MAX()**\n\nFinds maximum value.\n\n```foxpro\nSELECT MAX(salary) FROM employees\n```".to_string(),
        "ARRAY" => "**ARRAY**\n\nUsed with INTO ARRAY to store results.\n\n```foxpro\nSELECT * FROM customers ;\n    INTO ARRAY aCustomers\n```".to_string(),
        "NOFILTER" => "**NOFILTER**\n\nIgnores SET FILTER condition.\n\n```foxpro\nSELECT * FROM customers NOFILTER\n```".to_string(),
        "READWRITE" => "**READWRITE**\n\nCreates an updatable cursor.\n\n```foxpro\nSELECT * FROM customers ;\n    INTO CURSOR curEdit READWRITE\n```".to_string(),
        _ => return None,
    })
}

const VFP_KEYWORDS: &[&str] = &[
    "IF",
    "ELSE",
    "ELSEIF",
    "ENDIF",
    "DO",
    "WHILE",
    "ENDDO",
    "FOR",
    "ENDFOR",
    "NEXT",
    "TO",
    "STEP",
    "SCAN",
    "ENDSCAN",
    "CASE",
    "OTHERWISE",
    "ENDCASE",
    "TRY",
    "CATCH",
    "FINALLY",
    "ENDTRY",
    "THROW",
    "FUNCTION",
    "ENDFUNC",
    "PROCEDURE",
    "ENDPROC",
    "LPARAMETERS",
    "PARAMETERS",
    "LOCAL",
    "PRIVATE",
    "PUBLIC",
    "DIMENSION",
    "DEFINE",
    "CLASS",
    "ENDDEFINE",
    "AS",
    "OF",
    "THIS",
    "THISFORM",
    "DODEFAULT",
    "NODEFAULT",
    "RETURN",
    "EXIT",
    "LOOP",
    "WITH",
    "ENDWITH",
    "TEXT",
    "ENDTEXT",
    "USE",
    "CLOSE",
    "CLEAR",
    "RELEASE",
    "STORE",
];

const SQL_KEYWORDS: &[&str] = &[
    "SELECT", "FROM", "WHERE", "ORDER", "BY", "GROUP", "HAVING", "INTO", "CURSOR", "TABLE",
    "INSERT", "UPDATE", "DELETE", "SET", "VALUES", "JOIN", "INNER", "LEFT", "RIGHT", "OUTER", "ON",
    "DISTINCT", "TOP", "UNION", "ALL", "AND", "OR", "NOT", "LIKE", "BETWEEN", "IS", "NULL",
];

const BUILTIN_FUNCTIONS: &[&str] = &[
    "ABS",
    "ACLASS",
    "ACOPY",
    "ADIR",
    "AELEMENT",
    "AFIELDS",
    "ALEN",
    "ALIAS",
    "ALLTRIM",
    "ASC",
    "AT",
    "BETWEEN",
    "BOF",
    "CDOW",
    "CHR",
    "CMONTH",
    "CREATEOBJECT",
    "CTOD",
    "DATE",
    "DATETIME",
    "DAY",
    "DELETED",
    "DTOC",
    "DTOS",
    "EMPTY",
    "EOF",
    "EVALUATE",
    "FILE",
    "FOUND",
    "GETENV",
    "GOMONTH",
    "IIF",
    "INLIST",
    "INT",
    "ISDIGIT",
    "ISNULL",
    "LEFT",
    "LEN",
    "LOWER",
    "LTRIM",
    "MAX",
    "MESSAGEBOX",
    "MIN",
    "MOD",
    "MONTH",
    "NVL",
    "PADL",
    "PADR",
    "PROPER",
    "RAT",
    "RECCOUNT",
    "RECNO",
    "REPLICATE",
    "RIGHT",
    "ROUND",
    "RTRIM",
    "SECONDS",
    "SEEK",
    "SELECT",
    "SPACE",
    "SQRT",
    "STR",
    "STRTOFILE",
    "STUFF",
    "SUBSTR",
    "SYS",
    "TIME",
    "TRANSFORM",
    "TRIM",
    "TYPE",
    "UPPER",
    "USED",
    "VAL",
    "VARTYPE",
    "VERSION",
    "YEAR",
];
