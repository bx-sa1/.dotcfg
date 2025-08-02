local opt = vim.opt

opt.mouse = 'a'
vim.schedule(function()
  vim.opt.clipboard = 'unnamedplus'
end)
opt.undofile = true
opt.formatoptions = "jcroqlnt" -- tcqj

-- appearance
opt.smoothscroll = true
opt.showmode = false
opt.number = true
opt.wrap = false
opt.linebreak = true
opt.signcolumn = "yes"
opt.conceallevel = 2
opt.list = true
opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }

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
