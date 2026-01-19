local M = {}

M.config = {
  cmd = nil,
  filetypes = { "vfp", "foxpro" },
  root_patterns = { ".git", "*.pjx" },
  auto_install = true,
}

local function get_install_dir()
  return vim.fn.stdpath("data") .. "/vfp-lsp"
end

local function get_binary_name()
  if vim.fn.has("win32") == 1 then
    return "vfp-lsp-windows-x64.exe"
  elseif vim.fn.has("mac") == 1 then
    if vim.fn.system("uname -m"):match("arm64") then
      return "vfp-lsp-macos-arm64"
    else
      return "vfp-lsp-macos-x64"
    end
  else
    if vim.fn.system("uname -m"):match("aarch64") then
      return "vfp-lsp-linux-arm64"
    else
      return "vfp-lsp-linux-x64"
    end
  end
end

local function get_binary_path()
  return get_install_dir() .. "/" .. get_binary_name()
end

local function binary_exists()
  return vim.fn.filereadable(get_binary_path()) == 1
end

local function get_latest_release_url()
  local binary = get_binary_name()
  return "https://github.com/franpfeiffer/vfp-lsp/releases/latest/download/" .. binary
end

local function download_binary(callback)
  local install_dir = get_install_dir()
  local binary_path = get_binary_path()
  local url = get_latest_release_url()

  vim.fn.mkdir(install_dir, "p")

  vim.notify("VFP-LSP: Downloading binary...", vim.log.levels.INFO)

  local cmd
  if vim.fn.executable("curl") == 1 then
    cmd = { "curl", "-sL", "-o", binary_path, url }
  elseif vim.fn.executable("wget") == 1 then
    cmd = { "wget", "-q", "-O", binary_path, url }
  else
    vim.notify("VFP-LSP: curl or wget required to download binary", vim.log.levels.ERROR)
    return
  end

  vim.fn.jobstart(cmd, {
    on_exit = function(_, code)
      if code == 0 then
        if vim.fn.has("win32") == 0 then
          vim.fn.system({ "chmod", "+x", binary_path })
        end
        vim.notify("VFP-LSP: Binary installed successfully", vim.log.levels.INFO)
        if callback then
          callback()
        end
      else
        vim.notify("VFP-LSP: Failed to download binary", vim.log.levels.ERROR)
      end
    end,
  })
end

function M.setup(opts)
  opts = opts or {}
  M.config = vim.tbl_deep_extend("force", M.config, opts)

  local cmd = M.config.cmd
  if not cmd then
    if binary_exists() then
      cmd = get_binary_path()
    elseif vim.fn.executable("vfp-lsp") == 1 then
      cmd = "vfp-lsp"
    elseif M.config.auto_install then
      download_binary(function()
        M.start_lsp(get_binary_path())
      end)
      return
    else
      vim.notify("VFP-LSP: Binary not found. Run :VfpLspInstall", vim.log.levels.WARN)
      return
    end
  end

  M.start_lsp(cmd)
end

function M.start_lsp(cmd)
  if type(cmd) == "string" then
    cmd = { cmd }
  end

  local ok, lspconfig = pcall(require, "lspconfig")
  if not ok then
    M.setup_manual(cmd)
    return
  end

  local configs = require("lspconfig.configs")
  if not configs.vfp then
    configs.vfp = {
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

  lspconfig.vfp.setup({
    cmd = cmd,
    filetypes = M.config.filetypes,
    on_attach = M.config.on_attach,
    capabilities = M.config.capabilities,
  })
end

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

function M.install()
  download_binary(function()
    vim.notify("VFP-LSP: Restart Neovim to use the LSP", vim.log.levels.INFO)
  end)
end

function M.update()
  local binary_path = get_binary_path()
  if vim.fn.filereadable(binary_path) == 1 then
    vim.fn.delete(binary_path)
  end
  M.install()
end

vim.api.nvim_create_user_command("VfpLspInstall", M.install, {})
vim.api.nvim_create_user_command("VfpLspUpdate", M.update, {})

return M
