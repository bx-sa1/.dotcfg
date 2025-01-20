local opt = vim.opt

opt.number = true
opt.mouse = 'a'
opt.showmode = false
vim.schedule(function()
  vim.opt.clipboard = 'unnamedplus'
end)
opt.undofile = true
opt.wrap = false
opt.formatoptions = "jcroqlnt" -- tcqj
opt.linebreak = true

-- search 
opt.incsearch = true
opt.hlsearch = true
opt.ignorecase = true

-- performance
opt.autoread = true
opt.lazyredraw = true

-- tabs/spaces
opt.smartindent = true
opt.tabstop = 4
opt.expandtab = true
opt.shiftwidth = 4
