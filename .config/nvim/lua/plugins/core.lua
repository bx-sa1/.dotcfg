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
    keys = {
      { "<leader><C-\\>", "<C-\\>", remap = true},
      { "<leader><C-g>", "<C-g>", remap = true},
      { "<leader><C-k>", "<C-k>", remap = true},
      { "<leader><C-p>", "<C-p>", remap = true},
      { "<leader><C-l>", "<C-l>", remap = true},
    }
  }
}
