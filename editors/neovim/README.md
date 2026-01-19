# VFP-LSP for Neovim

This directory contains Neovim configuration for the Visual FoxPro Language Server.

## Installation

### Using lazy.nvim

```lua
{
  "franpfeiffer/fxp-lsp",
  config = function()
    require("vfp-lsp").setup({
      -- Optional: specify path to vfp-lsp if not in PATH
      -- cmd = "/path/to/vfp-lsp",
    })
  end,
  ft = { "vfp", "foxpro" },
}
```

### Using packer.nvim

```lua
use {
  "franpfeiffer/fxp-lsp",
  config = function()
    require("vfp-lsp").setup()
  end,
  ft = { "vfp", "foxpro" },
}
```

### Manual Installation

1. Build the LSP server:
   ```bash
   cargo build --release -p vfp-lsp
   ```

2. Add the binary to your PATH or specify its location in setup.

3. Add this to your Neovim config:
   ```lua
   -- In your init.lua
   require("vfp-lsp").setup({
     cmd = "/path/to/target/release/vfp-lsp",
   })
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
