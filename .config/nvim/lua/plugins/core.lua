return {
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 300
		end,
		opts = {
			-- your configuration comes here
			-- or leave it empty to use the default settings
			-- refer to the configuration section below
		}
	},
  {
    "ibhagwan/fzf-lua",
    keys = function()
      local fzf_lua = require('fzf-lua')
      return {
        { "<leader>fc", function() fzf_lua.files({ cwd = "~/.config/nvim/" }) end, remap = true, desc = "Find Config file" },
        { "<leader>ff", function() fzf_lua.files() end, remap = true, desc = "Find File" },
        { "<leader>bb", function() fzf_lua.buffers() end, remap = true, desc = "Find Buffer" },
        { "<leader>t", function() fzf_lua.tabs() end, remap = true, desc = "List tabs" },
        { "<leader>cq", function() fzf_lua.quickfix() end, remap = true, desc = "Show quickfixes" },
        { "<leader>cg", function() fzf_lua.grep() end, remap = true, desc = "Grep Buffer" },
        { "<leader>cs", function() fzf_lua.git_status() end, remap = true, desc = "Show Current buffers Git Status" },
        { "<leader>cm", function() fzf_lua.man_pages() end, remap = true, desc = "Show Manpage" },
        { "<leader>clr", function() fzf_lua.lsp_references() end, remap = true, desc = "Show LSP References" },
        { "<leader>cla", function() fzf_lua.lsp_code_actions() end, remap = true, desc = "Show LSP Code Actions" },
        { "<leader>clf", function() fzf_lua.lsp_definitions() end, remap = true, desc = "Show LSP Definitions" },
        { "<leader>clc", function() fzf_lua.lsp_declarations() end, remap = true, desc = "Show LSP Declarations" },
        { "<leader>clt", function() fzf_lua.lsp_typedefs() end, remap = true, desc = "Show LSP Typedefs" },
        { "<leader>cli", function() fzf_lua.lsp_implementations() end, remap = true, desc = "Show LSP Implementations" },
      }
    end
  }
}
