//! LSP backend implementation.

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use vfp_lexer::{TokenKind, is_keyword, is_sql_keyword};

use crate::capabilities::server_capabilities;
use crate::document::DocumentStore;

/// The main LSP backend.
pub struct Backend {
    client: Client,
    documents: DocumentStore,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DocumentStore::new(),
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

    fn get_builtin_definition(&self, word: &str) -> Option<(String, String)> {
        match word {
            "SELECT" => Some((
                "SELECT [fields] FROM [table] WHERE [condition]".to_string(),
                "SQL SELECT command - retrieves data from tables".to_string(),
            )),
            "FUNCTION" => Some((
                "FUNCTION FunctionName([parameters]) AS ReturnType".to_string(),
                "Defines a user-defined function".to_string(),
            )),
            "PROCEDURE" => Some((
                "PROCEDURE ProcedureName([parameters])".to_string(),
                "Defines a user-defined procedure".to_string(),
            )),
            "IF" => Some((
                "IF [condition] [commands] ELSE [commands] ENDIF".to_string(),
                "Conditional execution".to_string(),
            )),
            "FOR" => Some((
                "FOR variable = start TO end [STEP increment] [commands] ENDFOR".to_string(),
                "Loop with counter".to_string(),
            )),
            "WHILE" => Some((
                "WHILE [condition] [commands] ENDWHILE".to_string(),
                "Conditional loop".to_string(),
            )),
            "RETURN" => Some((
                "RETURN [expression]".to_string(),
                "Returns from function/procedure with optional value".to_string(),
            )),
            "DO" => Some((
                "DO ProcedureName [WITH parameters]".to_string(),
                "Calls a procedure".to_string(),
            )),
            _ => None,
        }
    }

    /// Publish diagnostics for a document.
    async fn publish_diagnostics(&self, uri: Url) {
        if let Some(doc) = self.documents.get(&uri) {
            let diagnostics = doc.diagnostics();
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
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
        self.publish_diagnostics(uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().next() {
            self.documents.update(&uri, change.text);
            self.publish_diagnostics(uri).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.documents.update(&params.text_document.uri, text);
            self.publish_diagnostics(params.text_document.uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.close(&params.text_document.uri);
        // Clear diagnostics
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
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

        // Provide hover info for keywords
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
        let position = params.text_document_position.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = doc.position_to_offset(position);

        // Get context from trigger character
        let trigger = params
            .context
            .and_then(|c| c.trigger_character)
            .unwrap_or_default();

        let mut completions = Vec::new();

        if trigger == "#" {
            // Preprocessor directives
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
            // After a dot, suggest boolean literals and logical operators
            for item in &[".T.", ".F.", ".NULL.", ".AND.", ".OR.", ".NOT."] {
                completions.push(CompletionItem {
                    label: item.to_string(),
                    kind: Some(CompletionItemKind::CONSTANT),
                    ..Default::default()
                });
            }
        } else {
            // General completions - keywords
            for keyword in VFP_KEYWORDS {
                completions.push(CompletionItem {
                    label: keyword.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                });
            }

            // SQL keywords
            for keyword in SQL_KEYWORDS {
                completions.push(CompletionItem {
                    label: keyword.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("SQL".to_string()),
                    ..Default::default()
                });
            }

            // Built-in functions
            for func in BUILTIN_FUNCTIONS {
                completions.push(CompletionItem {
                    label: format!("{}()", func),
                    kind: Some(CompletionItemKind::FUNCTION),
                    insert_text: Some(format!("{}($0)", func)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }

            // Add identifiers from current document
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

        // Search for function/procedure/class definitions
        let symbols = doc.symbols();
        for symbol in symbols {
            if symbol.name.eq_ignore_ascii_case(&word) {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                    uri.clone(),
                    symbol.selection_range,
                ))));
            }
        }

        // Check if it's a built-in keyword and create a definition in the current document
        let upper_word = word.to_ascii_uppercase();
        if let Some((_definition_line, _description)) = self.get_builtin_definition(&upper_word) {
            // For now, don't provide go-to-definition for built-in keywords
            // The hover functionality already shows the documentation
            return Ok(None);
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
        let mut offset = 0;

        for token in &doc.tokens {
            if token.kind == TokenKind::Ident {
                let text = &doc.content[offset..offset + token.len as usize];
                if text.eq_ignore_ascii_case(&word) {
                    let start = doc.offset_to_position(offset);
                    let end = doc.offset_to_position(offset + token.len as usize);
                    locations.push(Location::new(uri.clone(), Range::new(start, end)));
                }
            }
            offset += token.len as usize;
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
                    Some(17) // comment
                }
                TokenKind::Literal { kind } => match kind {
                    vfp_lexer::LiteralKind::StringDouble { .. }
                    | vfp_lexer::LiteralKind::StringSingle { .. }
                    | vfp_lexer::LiteralKind::StringBracket { .. } => Some(18), // string
                    vfp_lexer::LiteralKind::Int
                    | vfp_lexer::LiteralKind::Float
                    | vfp_lexer::LiteralKind::Hex
                    | vfp_lexer::LiteralKind::Date
                    | vfp_lexer::LiteralKind::DateTime => Some(19), // number
                },
                TokenKind::True | TokenKind::False | TokenKind::Null => Some(19), // number (constants)
                TokenKind::PreDefine
                | TokenKind::PreUndef
                | TokenKind::PreIf
                | TokenKind::PreIfDef
                | TokenKind::PreIfNDef
                | TokenKind::PreElse
                | TokenKind::PreElif
                | TokenKind::PreEndIf
                | TokenKind::PreInclude => Some(14), // macro
                TokenKind::DotAnd | TokenKind::DotOr | TokenKind::DotNot => Some(21), // operator
                TokenKind::Ident => {
                    let text = &doc.content[offset..offset + token.len as usize];
                    if is_keyword(text) {
                        Some(15) // keyword
                    } else if is_sql_keyword(text) {
                        Some(15) // keyword (SQL)
                    } else {
                        None // identifier - no semantic token
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
}

/// Get documentation for a VFP keyword.
fn get_keyword_documentation(keyword: &str) -> Option<String> {
    Some(match keyword {
        "FUNCTION" => "**FUNCTION**\n\nDefines a user-defined function.\n\n```foxpro\nFUNCTION name(parameters)\n    * code\n    RETURN value\nENDFUNC\n```".to_string(),
        "PROCEDURE" => "**PROCEDURE**\n\nDefines a procedure (subroutine).\n\n```foxpro\nPROCEDURE name\n    LPARAMETERS param1, param2\n    * code\nENDPROC\n```".to_string(),
        "IF" => "**IF**\n\nConditional execution.\n\n```foxpro\nIF condition\n    * code\nELSE\n    * code\nENDIF\n```".to_string(),
        "DO" => "**DO**\n\nExecute a program, procedure, or loop.\n\n```foxpro\nDO WHILE condition\n    * code\nENDDO\n\nDO CASE\n    CASE condition\n        * code\nENDCASE\n```".to_string(),
        "FOR" => "**FOR**\n\nLoop through a range of values.\n\n```foxpro\nFOR i = 1 TO 10 STEP 1\n    * code\nENDFOR\n```".to_string(),
        "SCAN" => "**SCAN**\n\nLoop through records in a table.\n\n```foxpro\nSCAN\n    * process each record\nENDSCAN\n```".to_string(),
        "LOCAL" => "**LOCAL**\n\nDeclare local variables.\n\n```foxpro\nLOCAL cName, nValue, aArray[10]\n```".to_string(),
        "PRIVATE" => "**PRIVATE**\n\nDeclare private variables (hidden from higher-level routines).\n\n```foxpro\nPRIVATE cName, nValue\n```".to_string(),
        "PUBLIC" => "**PUBLIC**\n\nDeclare public variables (accessible everywhere).\n\n```foxpro\nPUBLIC gAppName, gUserId\n```".to_string(),
        "DEFINE" => "**DEFINE CLASS**\n\nDefine a new class.\n\n```foxpro\nDEFINE CLASS MyClass AS ParentClass\n    Property = value\n    \n    PROCEDURE Method\n        * code\n    ENDPROC\nENDDEFINE\n```".to_string(),
        "TRY" => "**TRY**\n\nStructured error handling.\n\n```foxpro\nTRY\n    * code that might fail\nCATCH TO oException\n    * handle error\nFINALLY\n    * cleanup\nENDTRY\n```".to_string(),
        "WITH" => "**WITH**\n\nSet object context for property access.\n\n```foxpro\nWITH thisform\n    .Caption = \"Title\"\n    .Width = 500\nENDWITH\n```".to_string(),
        "RETURN" => "**RETURN**\n\nReturn from a function or procedure.\n\n```foxpro\nRETURN expression\n```".to_string(),
        "THIS" => "**THIS**\n\nReference to the current object instance.".to_string(),
        "THISFORM" => "**THISFORM**\n\nReference to the form containing the current object.".to_string(),
        _ => return None,
    })
}

/// Get documentation for an SQL keyword.
fn get_sql_keyword_documentation(keyword: &str) -> Option<String> {
    Some(match keyword {
        "SELECT" => "**SELECT**\n\nRetrieve data from tables.\n\n```foxpro\nSELECT columns FROM table ;\n    WHERE condition ;\n    ORDER BY column ;\n    INTO CURSOR cursorname\n```".to_string(),
        "INSERT" => "**INSERT**\n\nInsert new records.\n\n```foxpro\nINSERT INTO table (col1, col2) ;\n    VALUES (val1, val2)\n```".to_string(),
        "UPDATE" => "**UPDATE**\n\nUpdate existing records.\n\n```foxpro\nUPDATE table ;\n    SET column = value ;\n    WHERE condition\n```".to_string(),
        "DELETE" => "**DELETE**\n\nDelete records from a table.\n\n```foxpro\nDELETE FROM table WHERE condition\n```".to_string(),
        "JOIN" => "**JOIN**\n\nCombine rows from multiple tables.\n\n```foxpro\nSELECT * FROM table1 ;\n    INNER JOIN table2 ON table1.id = table2.id\n```".to_string(),
        _ => return None,
    })
}

/// Common VFP keywords.
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

/// SQL keywords.
const SQL_KEYWORDS: &[&str] = &[
    "SELECT", "FROM", "WHERE", "ORDER", "BY", "GROUP", "HAVING", "INTO", "CURSOR", "TABLE",
    "INSERT", "UPDATE", "DELETE", "SET", "VALUES", "JOIN", "INNER", "LEFT", "RIGHT", "OUTER", "ON",
    "DISTINCT", "TOP", "UNION", "ALL", "AND", "OR", "NOT", "LIKE", "BETWEEN", "IS", "NULL",
];

/// Built-in VFP functions.
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
