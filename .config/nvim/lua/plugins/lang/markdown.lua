return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "markdown" } },
  },

  {
    "williamboman/mason.nvim",
    opts = { ensure_installed = { "marksman" } },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        marksman = {},
      },
    },
  },
}
