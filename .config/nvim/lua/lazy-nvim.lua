-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out,                            "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

-- Setup lazy.nvim
require("lazy").setup({
    spec = {
        -- default plugins

        -- colorscheme
        {
            "dylanaraps/wal.vim",
            priority = 1000,
            init = function()
                vim.opt.termguicolors = false
                vim.cmd.colorscheme 'wal'
                vim.cmd.highlight 'Normal ctermbg=0'
            end,
        },

        -- fzf-lua
        {
            'ibhagwan/fzf-lua',
            config = function(opts)
                local fzf = require('fzf-lua')
                local map = vim.keymap.set
                map('n', '<leader>sf', function() fzf.files() end, { desc = '[S]earch for [f]ile' })
                map('n', '<leader>so', function() fzf.oldfiles() end, { desc = '[S]earch [o]ld files' })
                map('n', '<leader>sk', function() fzf.keymaps() end, { desc = '[S]earch [k]eymaps' })
                map('n', '<leader>sb', function() fzf.buffers() end, { desc = '[S]earch for [b]uffers' })
                map('n', '<leader>sg', function() fzf.live_grep_native() end,
                    { desc = '[S]earch for word using live_[g]rep' })
                map('n', '<leader>sc', function() fzf.files({ cwd = vim.fn.stdpath 'config' }) end,
                    { desc = '[S]earch for files in Neoovim config dir' })
                map('n', '<leader>/', function() fzf.grep_curbuf() end, { desc = '[/] Fuzzy search in current buffer' })
            end
        },

        -- mini.nvim
        {
            'echasnovski/mini.nvim',
            version = '*',
            config = function()
                -- mini.completion
                require('mini.completion').setup({
                    fallback_action = "<C-x><C-o>",
                })

                -- mini.pairs
                require('mini.pairs').setup()

                -- mini.statusline
                require('mini.statusline').setup()

                -- mini.icons
                require('mini.icons').setup()
            end,
        },

        -- which-key
        {
            'folke/which-key.nvim',
            preset = 'modern',
            event = 'VimEnter',
            opts = {
                delay = 0,
                spec = {
                    { '<leader>s', group = '[S]earch' },
                    { '<leader>c', group = '[C]ode', mode = { 'n', 'x' } },
                    { '<leader>w', group = '[W]indows', proxy = '<C-w>', },


                }
            }
        },

        -- lspconfig
        {
            'neovim/nvim-lspconfig',
            dependencies = {
                { "williamboman/mason.nvim", opts = { ensure_installed = { 'stylua', 'shfmt' } } },
                "williamboman/mason-lspconfig.nvim",
            },
            config = function()
                vim.api.nvim_create_autocmd('LspAttach', {
                    group = vim.api.nvim_create_augroup('kickstart-lsp-attach', { clear = true }),
                    callback = function(event)
                        local map = function(keys, func, desc, mode)
                            mode = mode or 'n'
                            vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
                        end
                        local fzf = require('fzf-lua')

                        map('gd', function() fzf.lsp_definitions() end, '[G]oto [D]efinition')
                        map('gr', function() fzf.lsp_references() end, '[G]oto [R]eferences')
                        map('gI', function() fzf.lsp_implementations() end, '[G]oto [I]mplementation')
                        map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
                        map('gy', function() fzf.lsp_type_definitions() end, '[G]oto T[y]pe Definition')
                        map('<leader>cs', function() fzf.lsp_document_symbols() end, 'Document [S]ymbols')
                        map('<leader>cw', function() fzf.lsp_dynamic_workspace_symbols() end, '[W]orkspace Symbols')
                        map('<leader>cr', vim.lsp.buf.rename, '[R]ename')
                        map('<leader>ca', vim.lsp.buf.code_action, 'Code [A]ction', { 'n', 'x' })
                    end
                })

                local servers = require("lspconfig-servers")
                require("mason-lspconfig").setup {
                    automatic_installation = true,
                    handlers = {
                        function(server_name)
                            local server_conf = servers[server_name] or {}
                            require('lspconfig')[server_name].setup(server_conf)
                        end
                    }
                }
            end,
        },

        {
            "nvim-treesitter/nvim-treesitter",
            build = ':TSUpdate',
            main = 'nvim-treesitter.configs',
            opts = {
                auto_install = true,
                ensure_installed = { "bash", "lua" },
                highlight = { enable = true },
                indent = { enable = true }
            }
        },

        -- import your plugins
        { import = "plugins" },
    },
    -- automatically check for plugin updates
    checker = { enabled = true },
})
