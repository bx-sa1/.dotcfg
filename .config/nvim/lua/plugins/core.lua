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
        { "<leader>C", function() fzf_lua.colorschemes() end, remap = true, desc = "Show colorschemes" }
      }
    end
  },
  {
    "nvimdev/dashboard-nvim",
    event = "VimEnter",
    config = function()
      require("dashboard").setup({
        config = {
          header = {
              ' ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗',
              ' ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║',
              ' ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║',
              ' ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║',
              ' ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║',
              ' ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝',
          },
          project = { enable = false },
        }
      })
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    opts = {
      options = {
        icons_enabled = true,
        theme = 'auto',
        component_separators = '|',
        section_separators = '',
        disabled_filetypes = {
          statusline = {},
          winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = false,
        refresh = {
          statusline = 1000,
          tabline = 1000,
          winbar = 1000,
        }
      },
      sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', 'diff', 'diagnostics'},
        lualine_c = {'filename'},
        lualine_x = {'encoding', 'fileformat', 'filetype'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {'location'},
        lualine_y = {},
        lualine_z = {}
      },
      tabline = {},
      winbar = {},
      inactive_winbar = {},
      extensions = {}
    },
  },
  {
    "nvim-tree/nvim-web-devicons",
    lazy = true
  },
  {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    config = true
    -- use opts = {} for passing setup options
    -- this is equalent to setup({}) function
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    opts = {
    indent = {
      char = "│",
      tab_char = "│",
    },
    scope = { enabled = false },
    exclude = {
      filetypes = {
        "help",
        "alpha",
        "dashboard",
        "neo-tree",
        "Trouble",
        "trouble",
        "lazy",
        "mason",
        "notify",
        "toggleterm",
        "lazyterm",
      },
    },
  },
  }
}
