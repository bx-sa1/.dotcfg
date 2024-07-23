return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "c" } },
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        clangd = {},
      },
    },
  },
  {
    "williamboman/mason.nvim",
    opts = { ensure_installed = { "clangd" } },
  },
}
