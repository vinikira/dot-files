return {
    'nvim-orgmode/orgmode',
    dependencies = {
        'nvim-orgmode/telescope-orgmode.nvim',
        'nvim-orgmode/org-bullets.nvim',
    },
    event = 'VeryLazy',
    ft = { 'org' },
    config = function()
        -- Setup orgmode
        require('orgmode').setup({
            org_agenda_files = '~/org/agenda/*',
            org_default_notes_file = '~/org/notes.org',
        })
        require('org-bullets').setup()
        require('cmp').setup({
            sources = {
                { name = 'orgmode' }
            }
        })

        require("telescope").load_extension("orgmode")

        local ext = require("telescope").extensions.orgmode
        vim.keymap.set("n", "<leader>oh", ext.search_headings, { desc = "Org headlines" })

        -- Experimental LSP support
        vim.lsp.enable('org')
    end,
}
