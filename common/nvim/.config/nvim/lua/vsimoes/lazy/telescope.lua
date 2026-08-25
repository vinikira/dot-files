return {
	"nvim-telescope/telescope.nvim",
	tag = "v0.2.2",
	dependencies = { 'nvim-lua/plenary.nvim' },
	config = function()
		require("telescope").setup({})

		local builtin = require('telescope.builtin')
		vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Find files' })
		vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Grep files' })
		vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Find buffers' })
		vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Help tags' })
		vim.keymap.set('n', '<C-p>', builtin.git_files, { desc = 'Find Git files' })
		vim.keymap.set('n', '<leader>fws', function()
		    local word = vim.fn.expand("<cword>")
		    builtin.grep_string({ search = word })
		end, { desc = 'Grep word at point' })
		vim.keymap.set('n', '<leader>fWs', function()
		    local word = vim.fn.expand("<cWORD>")
		    builtin.grep_string({ search = word })
		end, { desc = 'Grep block at point' })
        vim.keymap.set('n', '<leader>fF', builtin.treesitter, { desc = 'List functions (treesitter)' })
	end
}
