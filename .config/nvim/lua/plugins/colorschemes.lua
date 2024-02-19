return {
  {
    "haystackandroid/carbonized",
    lazy = false,
    priority = 1000,
  },
  {
    "ajmwagar/vim-deus",
    lazy = false,
    priority = 1000,
    config = function(_, opts)
      vim.cmd([[colorscheme deus]])
    end
  }
}
