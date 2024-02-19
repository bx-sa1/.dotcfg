local opt = vim.opt

vim.g.autoformat = true
vim.g.loaded_netrwPlugin = 1

opt.mouse = "a"
opt.autowrite = true
opt.clipboard = "unnamedplus"
opt.confirm = true
opt.number = true
opt.relativenumber = true
opt.expandtab = true
opt.shiftround = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.undofile = true
opt.undolevels = 10000
opt.wrap = false
opt.cursorline = true
opt.termguicolors = true

if vim.fn.has("nvim-0.10") == 1 then
  opt.smoothscroll = true
end
