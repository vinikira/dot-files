return {
	"nvim-telescope/telescope.nvim",
	tag = "0.1.8",
	dependencies = { 'nvim-lua/plenary.nvim' },
	config = function()
		require("telescope").setup({})

		local builtin = require('telescope.builtin')
		vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
		vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
		vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
		vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
		vim.keymap.set('n', '<C-p>', builtin.git_files, {})
		vim.keymap.set('n', '<leader>pws', function()
		    local word = vim.fn.expand("<cword>")
		    builtin.grep_string({ search = word })
		end)
		vim.keymap.set('n', '<leader>pWs', function()
		    local word = vim.fn.expand("<cWORD>")
		    builtin.grep_string({ search = word })
		end)
		vim.keymap.set('n', '<leader>vh', builtin.help_tags, {})
	end
}
