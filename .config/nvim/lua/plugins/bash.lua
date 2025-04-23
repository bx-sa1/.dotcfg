return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "bash" } },
  },
  {
    "williamboman/mason.nvim",
    opts = { ensure_installed = { "bash-language-server", "shellcheck", "shfmt" } },
  },
}
