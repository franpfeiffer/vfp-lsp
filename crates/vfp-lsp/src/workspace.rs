//! Workspace-wide indexing and symbol management.

use crate::document::Document;
use dashmap::DashMap;
use std::path::PathBuf;
use tower_lsp::lsp_types::*;

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub kind: SymbolKind,
    pub range: Range,
    pub file_uri: Url,
}

pub struct WorkspaceIndex {
    symbols: DashMap<String, Vec<SymbolInfo>>,
    files: DashMap<Url, PathBuf>,
}

impl WorkspaceIndex {
    pub fn new() -> Self {
        Self {
            symbols: DashMap::new(),
            files: DashMap::new(),
        }
    }

    pub fn index_file(&self, uri: Url, doc: &Document) {
        let path = uri.to_file_path().ok();
        if let Some(p) = path {
            self.files.insert(uri.clone(), p);
        }

        let symbols = doc.symbols();
        for symbol in symbols {
            let name_upper = symbol.name.to_uppercase();
            let info = SymbolInfo {
                name: symbol.name.clone(),
                kind: symbol.kind,
                range: symbol.selection_range,
                file_uri: uri.clone(),
            };

            self.symbols
                .entry(name_upper)
                .or_insert_with(Vec::new)
                .push(info);
        }
    }

    pub fn remove_file(&self, uri: &Url) {
        self.files.remove(uri);

        self.symbols.retain(|_, infos| {
            infos.retain(|info| &info.file_uri != uri);
            !infos.is_empty()
        });
    }

    pub fn find_symbol(&self, name: &str) -> Option<Vec<SymbolInfo>> {
        let upper = name.to_uppercase();
        self.symbols.get(&upper).map(|v| v.clone())
    }

    pub fn workspace_symbols(&self, query: &str) -> Vec<SymbolInformation> {
        let query_upper = query.to_uppercase();
        let mut results = Vec::new();

        for entry in self.symbols.iter() {
            for info in entry.value() {
                if query.is_empty() || info.name.to_uppercase().contains(&query_upper) {
                    #[allow(deprecated)]
                    results.push(SymbolInformation {
                        name: info.name.clone(),
                        kind: info.kind,
                        tags: None,
                        deprecated: None,
                        location: Location::new(info.file_uri.clone(), info.range),
                        container_name: None,
                    });
                }
            }
        }

        results
    }
}

impl Default for WorkspaceIndex {
    fn default() -> Self {
        Self::new()
    }
}
