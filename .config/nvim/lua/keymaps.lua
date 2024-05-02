vim.g.mapleader = " "
vim.g.maplocalleader = ","

local map = vim.keymap.set

map({ "n" }, "<Up>", "<Nop>", { remap = true })
map({ "n" }, "<Down>", "<Nop>", { remap = true })
map({ "n" }, "<Left>", "<Nop>", { remap = true })
map({ "n" }, "<Right>", "<Nop>", { remap = true })

map({ "n" }, "<leader>T", "<cmd>split term://bash<cr>", { desc = "Open Terminal", remap = true, silent = true })
map({ "n" }, "[b", "<cmd>bp<cr>", { desc = "Goto previous buffer", remap = true, silent = true })
map({ "n" }, "]b", "<cmd>bn<cr>", { desc = "Goto next buffer", remap = true, silent = true })
map({ "n" }, "<leader>L", "<cmd>Lazy<cr>", {desc = "Open lazy.nvim dialog", remap = true, silent = true })


-- Move Lines
map("n", "<A-j>", "<cmd>m .+1<cr>==", { desc = "Move Down" })
map("n", "<A-k>", "<cmd>m .-2<cr>==", { desc = "Move Up" })
map("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
map("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
map("v", "<A-j>", ":m '>+1<cr>gv=gv", { desc = "Move Down" })
map("v", "<A-k>", ":m '<-2<cr>gv=gv", { desc = "Move Up" })


-- Clear search with <esc>
map({ "i", "n" }, "<esc>", "<cmd>noh<cr><esc>", { desc = "Escape and Clear hlsearch" })

-- new file
map("n", "<leader>fn", "<cmd>enew<cr>", { desc = "New File" })
