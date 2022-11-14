local Remap = require('vsimoes.keymap')
local nnoremap = Remap.nnoremap
local inoremap = Remap.inoremap
local cmp = require('cmp')

cmp.setup({
    snippet = {
        expand = function(args)
            require('luasnip').lsp_expand(args.body)
        end,
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-y>'] = cmp.mapping.confirm({ select = true }),
        ['<C-f>'] = cmp.mapping.scroll_docs(-4),
        ['<C-b>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
    }),
    sources = {
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
        { name = 'buffer' },
    }
})

local function config(_config)
    return vim.tbl_deep_extend('force', {
        on_attach = function() 
            nnoremap('gd', function() vim.lsp.buf.definition() end)
            nnoremap('K', function() vim.lsp.buf.hover() end)
            nnoremap('<leader>vws', function() vim.lsp.buf.workspace_symbol() end)
            nnoremap('<leader>vd', function() vim.lsp.buf.open_float() end)
            nnoremap('<leader>[d', function() vim.lsp.buf.goto_next() end)
            nnoremap('<leader>]d', function() vim.lsp.buf.goto_prev() end)
            nnoremap('<leader>vca', function() vim.lsp.buf.code_action() end)
            nnoremap('<leader>vrr', function() vim.lsp.buf.references() end)
            nnoremap('<leader>vrn', function() vim.lsp.buf.rename() end)
            inoremap('<C-h>', function() vim.lsp.buf.signature_help() end)
        end
    }, _config or {})
end

require('lspconfig').tsserver.setup(config())
require('lspconfig').ccls.setup(config())
require('lspconfig').rust_analyzer.setup(config())
