# VFP-LSP for Neovim

This directory contains Neovim configuration for the Visual FoxPro Language Server.

## Installation

### Using lazy.nvim

```lua
return {
    {
        "franpfeiffer/vfp-lsp",
        ft = { "vfp", "foxpro" },
        config = function()
            require("vfp-lsp").setup()
        end,
    }
}
```

### Using packer.nvim

```lua
nobody uses packer anymore
```

### Manual Installation

1. Build the LSP server:
   ```bash
   cargo build --release -p vfp-lsp
   ```

2. Add the binary to your PATH or specify its location in setup.

3. Add this to your Neovim config:
```lua
return {
    {
        dir = "/your/path/to/vfp-lsp/editors/neovim",
        ft = { "vfp", "foxpro" },
        config = function()
            require("vfp-lsp").setup({
                cmd = { "/your/path/to/vfp-lsp/target/release/vfp-lsp" },
                auto_install = false,
            })
        end,
    }
}
```

## Configuration

```lua
require("vfp-lsp").setup({
  -- Path to vfp-lsp executable (default: find in PATH)
  cmd = nil,

  -- File types to attach to
  filetypes = { "vfp", "foxpro" },

  -- Root directory patterns
  root_patterns = { ".git", "*.pjx" },

  -- LSP on_attach callback
  on_attach = function(client, bufnr)
    -- Your keymaps and settings here
  end,

  -- LSP capabilities (for nvim-cmp integration, etc.)
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
})
```

## File Type Detection

The plugin automatically detects VFP files by extension:
- `.prg` - Program files
- `.fxp` - Compiled program files
- `.vcx/.vct` - Visual class library
- `.scx/.sct` - Form files
- `.frx/.frt` - Report files
- `.mnx/.mnt` - Menu files
- And more...

## Features

- Syntax highlighting
- Diagnostics (errors/warnings)
- Completion
- Go to definition
- Find references
- Hover information
- Document symbols (outline)
