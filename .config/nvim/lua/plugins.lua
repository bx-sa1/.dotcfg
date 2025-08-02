return {
    -- fzf-lua
    {
        'ibhagwan/fzf-lua',
        opts = {},
        keys = {
            { '<leader>sf', "<cmd>FzfLua files<cr>",                                                    desc = '[S]earch for [f]ile' },
            { '<leader>so', "<cmd>FzfLua oldfiles<cr>",                                                 desc = '[S]earch [o]ld files' },
            { '<leader>sk', "<cmd>FzfLua keymaps<cr>",                                                  desc = '[S]earch [k]eymaps' },
            { '<leader>sb', "<cmd>FzfLua buffers<cr>",                                                  desc = '[S]earch for [b]uffers' },
            { '<leader>st', "<cmd>FzfLua tabs<cr>",                                                     desc = '[S]earch for [t]abs' },
            { '<leader>sg', "<cmd>FzfLua grep_curbuf<cr>",                                              desc = '[S]earch for word using live_[g]rep' },
            { '<leader>sc', function() require('fzf-lua').files({ cwd = vim.fn.stdpath 'config' }) end, desc = '[S]earch for files in Neoovim config dir' },
            { '<leader>/',  "<cmd>FzfLua live_grep_native<cr>",                                         desc = '[/] Fuzzy search in current buffer' },
            { '<leader>ss', "<cmd>FzfLua lsp_document_symbols<cr>",                                     desc = 'Document [S]ymbols' },
            { '<leader>sS', "<cmd>FzfLua lsp_live_workspace_symbols<cr>",                               desc = 'Workspace [S]ymbols' }
        }
    },

    -- mini.completion
    {
        'echasnovski/mini.completion',
        opts = {
            fallback_action = "<C-x><C-n>",
        },
    },

    -- mini.pairs
    { 'echasnovski/mini.pairs',      config = true },

    -- mini.ai
    { 'echasnovski/mini.ai',         config = true },

    -- mini.statusline
    { 'echasnovski/mini.statusline', config = false },

    -- mini.icons
    { 'echasnovski/mini.icons',      config = true },

    -- mini.base16
    {
        'echasnovski/mini.base16',
        event = "VimEnter",
        opts = function()
            local f = assert(io.open(vim.fs.abspath("~/.cache/wal/colors.json")))
            local colors = vim.json.decode(f:read('*a'))
            f:close()

            local wal_palette = {}
            for k,v in pairs(colors["colors"]) do
                wal_palette["base" .. string.format("%02X", tonumber(string.sub(k, 6)))] = v
            end

            return {
                palette = wal_palette
            }
        end
    },

    -- mini.starter
    {
        "echasnovski/mini.starter",
        event = "VimEnter",
        opts = function()
            local starter = require("mini.starter")
            return {
                evaluate_single = true,
                header = [[
███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗
████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║
██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║
██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║
██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║
╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝
]],
                items = {
                    starter.sections.builtin_actions(),
                    starter.sections.recent_files(10, false),
                    starter.sections.recent_files(10, true),
                },
                content_hooks = {
                    starter.gen_hook.adding_bullet(),
                    starter.gen_hook.aligning('center', 'center'),
                },
            }
        end
    },

    -- which-key
    {
        'folke/which-key.nvim',
        event = "VeryLazy",
        preset = 'modern',
        opts = {
            delay = 0,
            spec = {
                {
                    mode = { "n", "v" },
                    { '<leader>s', group = '[S]earch' },
                    { '<leader>c', group = '[C]ode' },
                    {
                        "<leader>b",
                        group = "buffer",
                        expand = function()
                            return require("which-key.extras").expand.buf()
                        end,
                    },
                    {
                        "<leader>w",
                        group = "windows",
                        proxy = "<c-w>",
                        expand = function()
                            return require("which-key.extras").expand.win()
                        end,
                    },
                }
            }
        },
        keys = {
            {
                "<leader>?",
                function()
                    require("which-key").show({ global = false })
                end,
                desc = "Buffer Keymaps (which-key)",
            },
            {
                "<c-w><space>",
                function()
                    require("which-key").show({ keys = "<c-w>", loop = true })
                end,
                desc = "Window Hydra Mode (which-key)",
            },
        },
    },

    -- lspconfig
    {
        'neovim/nvim-lspconfig',
        dependencies = {
            "echasnovski/mini.completion"
        },
        config = function()
            vim.api.nvim_create_autocmd('LspAttach', {
                group = vim.api.nvim_create_augroup('LSP', { clear = true }),
                callback = function(event)
                    local map = function(keys, func, desc, mode)
                        mode = mode or 'n'
                        vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
                    end
                    local fzf = require('fzf-lua')

                    map('gd', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
                    map('gD', vim.lsp.buf.definition, '[G]oto [D]efinition')
                    map('gr', vim.lsp.buf.references, '[G]oto [R]eferences')
                    map('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
                    map('gy', vim.lsp.buf.type_definition, '[G]oto T[y]pe Definition')
                    map('K', function() return vim.lsp.buf.hover() end, 'Hover')
                    map('gK', function() return vim.lsp.buf.signature_help() end, 'Signature Help')
                    map('<c-k>', function() return vim.lsp.buf.signature_help() end, 'Signature Help', 'i')
                    map('<leader>ca', vim.lsp.buf.code_action, 'Code [A]ction', { 'n', 'x' })
                    map('<leader>cr', vim.lsp.buf.rename, '[R]ename')
                    map("<leader>cf", function() vim.lsp.buf.format({ async = true }) end, "[F]ormat buffer")

                    map("<leader>cD", function() fzf.lsp_workspace_diagnostics() end, "Workspace [D]iagnostics")
                end
            })

            local servers = require("lsp")
            for server, opts in pairs(servers) do
                opts.capabilities = vim.tbl_deep_extend('force',
                    vim.lsp.protocol.make_client_capabilities(),
                    MiniCompletion.get_lsp_capabilities(),
                    opts.capabilities or {})
                vim.lsp.enable(server)
                vim.lsp.config(server, opts)
            end
        end,
    },

    {
        "nvim-treesitter/nvim-treesitter",
        build = function()
            require("nvim-treesitter.install").update({ with_sync = true })()
        end,
        main = 'nvim-treesitter.configs',
        opts = {
            auto_install = true,
            ensure_installed = { "bash", "lua" },
            highlight = { enable = true },
            indent = { enable = true }
        }
    },
}
