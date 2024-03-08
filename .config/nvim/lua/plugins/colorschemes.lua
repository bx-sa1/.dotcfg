return {
  {
    "haystackandroid/carbonized",
    lazy = false,
    priority = 1000
  },
  {
    "ajmwagar/vim-deus",
    lazy = false,
    priority = 1000
  },
  {
    "aditya-azad/candle-grey",
    lazy = false,
    priority = 1000
  },
  {
    "rafi/awesome-vim-colorschemes",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd([[colorscheme deep-space]])
    end
  }
}
