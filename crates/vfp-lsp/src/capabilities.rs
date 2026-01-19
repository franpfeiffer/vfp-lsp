//! LSP server capabilities configuration.

use tower_lsp::lsp_types::*;

/// Build the server capabilities to advertise to the client.
pub fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        // Text document sync
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(true),
                })),
            },
        )),

        // Hover support
        hover_provider: Some(HoverProviderCapability::Simple(true)),

        // Completion support
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![".".to_string(), "#".to_string()]),
            all_commit_characters: None,
            work_done_progress_options: WorkDoneProgressOptions::default(),
            completion_item: None,
        }),

        // Definition support (go to definition)
        definition_provider: Some(OneOf::Left(true)),

        // References support (find all references)
        references_provider: Some(OneOf::Left(true)),

        // Document symbol support (outline)
        document_symbol_provider: Some(OneOf::Left(true)),

        // Workspace symbol support
        workspace_symbol_provider: Some(OneOf::Left(true)),

        // Signature help
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),

        // Semantic tokens (for syntax highlighting)
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions::default(),
                legend: SemanticTokensLegend {
                    token_types: semantic_token_types(),
                    token_modifiers: semantic_token_modifiers(),
                },
                range: Some(false),
                full: Some(SemanticTokensFullOptions::Bool(true)),
            },
        )),

        // Other capabilities we might add later
        rename_provider: None,
        document_formatting_provider: None,
        code_action_provider: None,
        code_lens_provider: None,
        document_highlight_provider: None,
        folding_range_provider: None,
        execute_command_provider: None,
        workspace: None,
        call_hierarchy_provider: None,
        inlay_hint_provider: None,
        inline_value_provider: None,
        linked_editing_range_provider: None,
        moniker_provider: None,
        position_encoding: None,
        selection_range_provider: None,
        type_definition_provider: None,
        implementation_provider: None,
        declaration_provider: None,
        diagnostic_provider: None,
        document_link_provider: None,
        color_provider: None,
        document_on_type_formatting_provider: None,
        document_range_formatting_provider: None,
        experimental: None,
    }
}

/// Semantic token types we support.
fn semantic_token_types() -> Vec<SemanticTokenType> {
    vec![
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::TYPE,
        SemanticTokenType::CLASS,
        SemanticTokenType::ENUM,
        SemanticTokenType::INTERFACE,
        SemanticTokenType::STRUCT,
        SemanticTokenType::TYPE_PARAMETER,
        SemanticTokenType::PARAMETER,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::EVENT,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::METHOD,
        SemanticTokenType::MACRO,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::MODIFIER,
        SemanticTokenType::COMMENT,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::REGEXP,
        SemanticTokenType::OPERATOR,
        SemanticTokenType::DECORATOR,
    ]
}

/// Semantic token modifiers we support.
fn semantic_token_modifiers() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DECLARATION,
        SemanticTokenModifier::DEFINITION,
        SemanticTokenModifier::READONLY,
        SemanticTokenModifier::STATIC,
        SemanticTokenModifier::DEPRECATED,
        SemanticTokenModifier::ABSTRACT,
        SemanticTokenModifier::ASYNC,
        SemanticTokenModifier::MODIFICATION,
        SemanticTokenModifier::DOCUMENTATION,
        SemanticTokenModifier::DEFAULT_LIBRARY,
    ]
}
