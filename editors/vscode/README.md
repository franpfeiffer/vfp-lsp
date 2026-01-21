# Visual FoxPro Language Support

Language support for Visual FoxPro (VFP) in Visual Studio Code, powered by VFP-LSP.

## Features

- **Syntax Highlighting** - Full syntax highlighting for VFP code
- **Diagnostics** - Real-time error detection
- **IntelliSense** - Code completion for keywords, SQL, and functions
- **Go to Definition** - Jump to function/procedure/class definitions
- **Find All References** - Find all usages of symbols
- **Hover Documentation** - Documentation for 120+ keywords
- **Document Symbols** - Outline view of functions, procedures, and classes
- **Semantic Highlighting** - Enhanced syntax coloring

## Supported File Types

- `.prg` - Program files
- `.fxp` - Compiled programs
- `.spr`, `.mpr`, `.qpr` - Report/menu programs
- `.vcx`, `.vct` - Visual class libraries
- `.scx`, `.sct` - Forms
- `.frx`, `.frt` - Reports
- `.lbx`, `.lbt` - Labels
- `.mnx`, `.mnt` - Menus
- `.dbc`, `.dbf` - Database files

## Installation

1. Install this extension from the VS Code Marketplace
2. The language server will be automatically downloaded on first use
3. Open any VFP file and start coding!

### Manual Installation

If automatic installation fails:

1. Download the appropriate binary from [releases](https://github.com/franpfeiffer/vfp-lsp/releases)
2. Configure the path in settings (see below)

## Configuration

### Settings

```json
{
  // Path to vfp-lsp executable (optional)
  "vfp.server.path": "/path/to/vfp-lsp",
  
  // Enable verbose logging for debugging
  "vfp.trace.server": "verbose"
}
```

### Commands

- `VFP: Restart Language Server` - Restart the LSP server

## VFP Language Features

This extension supports modern and legacy VFP syntax:

- Preprocessor directives (`#DEFINE`, `#IF`, `#INCLUDE`)
- Embedded SQL (SELECT, INSERT, UPDATE, DELETE)
- Classes and inheritance
- Exception handling (TRY/CATCH/FINALLY)
- All control structures (IF, FOR, WHILE, SCAN, DO CASE)
- TEXT...ENDTEXT blocks with text merging
- Date/DateTime literals
- All VFP operators (`.AND.`, `.OR.`, `.NOT.`)

## Requirements

- Visual Studio Code 1.75.0 or higher
- VFP-LSP language server (auto-installed)

## Known Issues

See [GitHub Issues](https://github.com/franpfeiffer/vfp-lsp/issues)

## Contributing

Contributions are welcome! See [CONTRIBUTING.md](https://github.com/franpfeiffer/vfp-lsp/blob/main/CONTRIBUTING.md)

## License

MIT - See [LICENSE](https://github.com/franpfeiffer/vfp-lsp/blob/main/LICENSE)

## Release Notes

See [CHANGELOG.md](https://github.com/franpfeiffer/vfp-lsp/blob/main/CHANGELOG.md)
