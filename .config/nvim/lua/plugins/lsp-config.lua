return {
	{
		"neovim/nvim-lspconfig",
		version = false,
		config = function(_, opts)
      --Enable (broadcasting) snippet capability for completion
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities.textDocument.completion.completionItem.snippetSupport = true

      require('lspconfig')["html"].setup {
        capabilities = capabilities,
      }
      require('lspconfig')["rust_analyzer"].setup{}
      require('lspconfig')["tsserver"].setup{}
      require('lspconfig')["clangd"].setup{}
      require('lspconfig')["svelte"].setup{}
      require('lspconfig')["eslint"].setup {
        on_attach = function(client, bufnr)
          vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = bufnr,
            command = "EslintFixAll",
          })
        end,
      }
			require('lspconfig')["lua_ls"].setup {
				on_init = function(client)
					local path = client.workspace_folders[1].name
					if not vim.loop.fs_stat(path .. '/.luarc.json') and not vim.loop.fs_stat(path .. '/.luarc.jsonc') then
						client.config.settings = vim.tbl_deep_extend('force',
							client.config.settings, {
								Lua = {
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
											-- "${3rd}/luv/library"
											-- "${3rd}/busted/library",
										}
										-- or pull in all of 'runtimepath'. NOTE: this is a lot slower
										-- library = vim.api.nvim_get_runtime_file("", true)
									}
								}
							})

						client.notify("workspace/didChangeConfiguration",
							{ settings = client.config.settings })
					end
					return true
				end
			}

      local fzf = require('fzf-lua')

      -- Global mappings.
      -- See `:help vim.diagnostic.*` for documentation on any of the below functions
      vim.keymap.set('n', '<leader>cd', function() fzf.workspace_diagnostics() end, { desc = "Workspace Diagnostics" })
      vim.keymap.set('n', '<leader>cd', function() fzf.document_diagnostics() end, { desc = "Document Diagnostics" })
      vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
      vim.keymap.set('n', ']d', vim.diagnostic.goto_next)


      -- Use LspAttach autocommand to only map the following keys
      -- after the language server attaches to the current buffer
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          -- Enable completion triggered by <c-x><c-o>
          vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          -- Buffer local mappings.
          -- See `:help vim.lsp.*` for documentation on any of the below functions
          local opts = function (d)
            return { buffer = ev.buf, desc = d }
          end

          vim.keymap.set('n', '<localleader>gD', function() fzf.lsp_declarations() end, opts("Find Declarations"))
          vim.keymap.set('n', '<localleader>gd', function() fzf.lsp_definitions() end, opts("Find Definitions"))
          vim.keymap.set('n', '<localleader>gr', function() fzf.lsp_references() end, opts("Find References"))
          vim.keymap.set('n', '<localleader>gi', function() fzf.lsp_implementations() end, opts("Find Implementation"))
          vim.keymap.set('n', '<localleader>K', vim.lsp.buf.hover, opts("Hover"))
          vim.keymap.set('n', '<localleader><C-k>', vim.lsp.buf.signature_help, opts("Signature Help"))
          vim.keymap.set('n', '<localleader><space>wa', vim.lsp.buf.add_workspace_folder, opts("Add Workspace Folder"))
          vim.keymap.set('n', '<localleader><space>wr', vim.lsp.buf.remove_workspace_folder, opts("Remove Workspace Folder"))
          vim.keymap.set('n', '<localleader><space>wl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
          end, opts("List Workspace Folders"))
          vim.keymap.set('n', '<localleader><space>D', function() fzf.lsp_type_definition() end, opts("Find Type Definitions"))
          vim.keymap.set('n', '<localleader><space>rn', vim.lsp.buf.rename, opts("Rename"))
          vim.keymap.set({ 'n', 'v' }, '<localleader><space>ca', function() fzf.lsp_code_actions() end, opts("Code Action"))
          vim.keymap.set('n', '<localleader><space>f', function()
            vim.lsp.buf.format { async = true }
          end, opts("Format"))
          vim.keymap.set('n', '<localleader>c', function() fzf.complete_bline() end, opts("Complete line"))
        end,
      })
		end
	}
}
