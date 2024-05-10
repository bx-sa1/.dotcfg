-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

local function augroup(name, clear)
  return vim.api.nvim_create_augroup("lazyvim_" .. name, { clear = clear })
end

local autocmd = vim.api.nvim_create_autocmd

autocmd({ "BufRead", "BufWritePost" }, {
  group = augroup("CSV_Editing", false),
  pattern = "*.csv",
  command = ":%ArrangeColumn",
})

autocmd("BufWritePre", {
  group = augroup("CSV_Editing", false),
  pattern = "*.csv",
  command = ":%UnArrangeColumn",
})
