require("vsimoes.remap")
require("vsimoes.set")
require("vsimoes.lazy_init")

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
local VSimoesGroup = augroup('VSimoesGroup', {})

autocmd('LspAttach', {
    group = VSimoesGroup,
    callback = function(e)
        vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, { buffer = e.buff, desc = "Go to definition" })
        vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, { buffer = e.buff, desc = "Hover" })
        vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end,
            { buffer = e.buff, desc = "Workspace Symbol" })
        vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end,
            { buffer = e.buff, desc = "Diagnostic" })
        vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end,
            { buffer = e.buff, desc = "Code Action" })
        vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end,
            { buffer = e.buff, desc = "References" })
        vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, { buffer = e.buff, desc = "Rename" })
        vim.keymap.set("n", "<leader>vf", function() vim.lsp.buf.format() end, { buffer = e.buff, desc = "Format" })
        vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end,
            { buffer = e.buff, desc = "Signature Help" })
        vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, { buffer = e.buff, desc = "Next error" })
        vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, { buffer = e.buff, desc = "Previous error" })
    end
})
