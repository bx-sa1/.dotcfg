return {
    lua_ls = {
        on_init = function(client)
            if client.workspace_folders then
                local path = client.workspace_folders[1].name
                if vim.loop.fs_stat(path .. '/.luarc.json') or vim.loop.fs_stat(path .. '/.luarc.jsonc') then
                    return
                end
            end

            client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
                runtime = {
                    -- Tell the language server which version of Lua you're using
                    -- (most likely LuaJIT in the case of Neovim)
                    version = 'LuaJIT'
                },
                -- Make the server aware of Neovim runtime files
                workspace = {
                    checkThirdParty = false,
                    library = {
                        vim.env.VIMRUNTIME
                        -- Depending on the usage, you might want to add additional paths here.
                        -- "${3rd}/luv/library"
                        -- "${3rd}/busted/library",
                    }
                    -- or pull in all of 'runtimepath'. NOTE: this is a lot slower and will cause issues when working on your own configuration (see https://github.com/neovim/nvim-lspconfig/issues/3189)
                    -- library = vim.api.nvim_get_runtime_file("", true)
                }
            })
        end,
        settings = {
            Lua = {}
        }
    },
    marksman = {},
    clangd = {},
    rust_analyzer = {
        settings = {
            ['rust-analyzer'] = {
                cargo = {
                    features = "all",
                    allTargets = true,
                    extraArgs = { "--all" }
                },
                -- Add clippy lints for Rust.
                checkOnSave = true,
                check = {
                    command = "clippy",
                    extraArgs = {"--", "-W", "clippy::pedantic"},
                },
                procMacro = {
                    enable = true,
                    ignored = {
                        ["async-trait"] = { "async_trait" },
                        ["napi-derive"] = { "napi" },
                        ["async-recursion"] = { "async_recursion" },
                    },
                },
            }
        }
    },
    hls = {
        filetypes = { 'haskell', 'lhaskell', 'cabal' }
    },
    gopls = {},
    pyright = {},
    bashls = {},
    html = {},
    eslint = {
        on_attach = function(client, bufnr)
            vim.api.nvim_create_autocmd("BufWritePre", {
              buffer = bufnr,
              command = "EslintFixAll",
            })
          end,
    },
    cssls = {},
    jsonls = {},
    zk = {},
}
