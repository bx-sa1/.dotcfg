vim.g.mapleader = " "
vim.g.maplocalleader = ","

local map = vim.keymap.set

map({ "n" }, "<leader>T", "<cmd>split term://bash<cr>", { desc = "Open Terminal", remap = true, silent = true })
map({ "n" }, "[b", "<cmd>bp<cr>", { desc = "Goto previous buffer", remap = true, silent = true })
map({ "n" }, "]b", "<cmd>bn<cr>", { desc = "Goto next buffer", remap = true, silent = true })
map({ "n" }, "<leader>L", "<cmd>Lazy<cr>", {desc = "Open lazy.nvim dialog", remap = true, silent = true })

