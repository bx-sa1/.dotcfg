return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "go", "gomod", "gowork", "gosum" } },
  },

  {
    "williamboman/mason.nvim",
    opts = { ensure_installed = { "goimports", "gofumpt" } },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        gopls = {},
      },
    },
  },
}
