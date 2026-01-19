# VFP-LSP

A Language Server Protocol (LSP) implementation for Visual FoxPro.

## Features

- **Syntax Highlighting** - Full syntax highlighting for VFP code
- **Diagnostics** - Real-time error detection for syntax issues
- **Completion** - Keyword, function, and identifier completion
- **Go to Definition** - Navigate to function/procedure definitions
- **Find References** - Find all usages of a symbol
- **Hover** - Documentation on hover for keywords and built-in functions
- **Document Symbols** - Outline of functions, procedures, and classes
- **Semantic Tokens** - Enhanced syntax highlighting via LSP

## Supported VFP Features

- Preprocessor directives (`#DEFINE`, `#IF`, `#INCLUDE`, etc.)
- Embedded SQL (SELECT, INSERT, UPDATE, DELETE)
- Classes and inheritance
- Exception handling (TRY/CATCH/FINALLY)
- All control flow constructs
- Date/DateTime literals
- All VFP operators including `.AND.`, `.OR.`, `.NOT.`

## Building

### Prerequisites

- Rust 1.70 or later
- Node.js 18+ (for VS Code extension)

### Build the LSP Server

```bash
cargo build --release -p vfp-lsp
```

The binary will be at `target/release/vfp-lsp`.

### Run Tests

```bash
cargo test
```

## Editor Setup

### VS Code

1. Build the LSP server (see above)
2. Add `vfp-lsp` to your PATH, or configure the path in settings
3. Install the extension:
   ```bash
   cd editors/vscode
   npm install
   npm run compile
   ```
4. Press F5 to launch a new VS Code window with the extension loaded

### Neovim

See [editors/neovim/README.md](editors/neovim/README.md) for detailed instructions.

Quick setup with lazy.nvim:
```lua
{
  dir = "/path/to/vfp-lsp/editors/neovim",
  config = function()
    require("vfp-lsp").setup({
      cmd = "/path/to/target/release/vfp-lsp",
    })
  end,
  ft = { "vfp" },
}
```

## Configuration

### VS Code Settings

```json
{
  "vfp.server.path": "/path/to/vfp-lsp",
  "vfp.trace.server": "verbose"
}
```

### Neovim Configuration

```lua
require("vfp-lsp").setup({
  cmd = "/path/to/vfp-lsp",
  on_attach = function(client, bufnr)
    -- Your keymaps here
  end,
})
```

## Development

### Running the Lexer Example

```bash
cargo run -p vfp-lexer --example lex_file test-fixtures/sample.prg
```

### Testing the LSP Server

Start the server manually for testing:
```bash
cargo run -p vfp-lsp
```

## License

MIT
