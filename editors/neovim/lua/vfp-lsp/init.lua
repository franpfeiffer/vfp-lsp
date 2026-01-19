-- VFP LSP configuration for Neovim
-- This module provides LSP configuration for Visual FoxPro

local M = {}

-- Default configuration
M.config = {
  -- Path to the vfp-lsp executable (nil = find in PATH)
  cmd = nil,
  -- File patterns to attach to
  filetypes = { "vfp", "foxpro" },
  -- Root directory patterns
  root_patterns = { ".git", "*.pjx" },
}

-- Setup function to configure the LSP
function M.setup(opts)
  opts = opts or {}
  M.config = vim.tbl_deep_extend("force", M.config, opts)

  -- Determine the command to run
  local cmd = M.config.cmd or "vfp-lsp"
  if type(cmd) == "string" then
    cmd = { cmd }
  end

  -- Check if lspconfig is available
  local ok, lspconfig = pcall(require, "lspconfig")
  if not ok then
    -- Manual LSP configuration without lspconfig
    M.setup_manual(cmd)
    return
  end

  -- Check if vfp config already exists in lspconfig
  local configs = require("lspconfig.configs")
  if not configs.vfp_lsp then
    configs.vfp_lsp = {
      default_config = {
        cmd = cmd,
        filetypes = M.config.filetypes,
        root_dir = function(fname)
          return lspconfig.util.root_pattern(unpack(M.config.root_patterns))(fname)
            or lspconfig.util.find_git_ancestor(fname)
            or vim.fn.getcwd()
        end,
        settings = {},
        init_options = {},
      },
    }
  end

  -- Setup the LSP
  lspconfig.vfp_lsp.setup({
    cmd = cmd,
    filetypes = M.config.filetypes,
    on_attach = M.config.on_attach,
    capabilities = M.config.capabilities,
  })
end

-- Manual LSP setup without lspconfig
function M.setup_manual(cmd)
  vim.api.nvim_create_autocmd("FileType", {
    pattern = M.config.filetypes,
    callback = function()
      vim.lsp.start({
        name = "vfp-lsp",
        cmd = cmd,
        root_dir = vim.fn.getcwd(),
      })
    end,
  })
end

-- Function to check if the LSP server is available
function M.is_available()
  local cmd = M.config.cmd or "vfp-lsp"
  if type(cmd) == "table" then
    cmd = cmd[1]
  end
  return vim.fn.executable(cmd) == 1
end

-- Function to get server info
function M.info()
  local info = {
    name = "VFP Language Server",
    cmd = M.config.cmd or "vfp-lsp",
    available = M.is_available(),
    filetypes = M.config.filetypes,
  }
  return info
end

return M
