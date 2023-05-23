vim.g['test#strategy'] = 'neovim'

vim.keymap.set('n', '<leader>tt', vim.cmd.TestNearest)
vim.keymap.set('n', '<leader>tT', vim.cmd.TestFile)
vim.keymap.set('n', '<leader>ta', vim.cmd.TestSuite)
vim.keymap.set('n', '<leader>tl', vim.cmd.TestLast)
vim.keymap.set('n', '<leader>tg', vim.cmd.TestVisit)
