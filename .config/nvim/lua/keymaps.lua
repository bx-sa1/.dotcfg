vim.g.mapleader = " "
vim.g.maplocalleader = ","

local map = vim.keymap.set

map({ "n" }, "<leader>fe",  "<cmd>Lexplore<cr>", { desc = "Open File explorer", remap = true, silent = true})
