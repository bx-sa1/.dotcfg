return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "haskell" } },
  },

  {
    "williamboman/mason.nvim",
    opts = { ensure_installed = { "haskell-language-server" } },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        hls = {
          filetypes = { "haskell", "lhaskell", "cabal" },
        },
      },
    },
  },
}
