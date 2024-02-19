vim.g.mapleader = " "
vim.g.maplocalleader = ","

local map = vim.keymap.set

map({ "n" }, "<leader>fe",  "<cmd>Lexplore<cr>", { desc = "Open File explorer", remap = true, silent = true})
map({ "n" }, "<leader>fc", "<cmd>e ~/.config/nvim/<cr>", { desc = "Open config folder", remap = true, silent = true})
map({ "n" }, "<leader>T", "<cmd>terminal<cr>", { desc = "Open Terminal", remap = true, silent = true})
