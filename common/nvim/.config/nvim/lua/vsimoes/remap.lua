local nnoremap = require('vsimoes.keymap').nnoremap
local inoremap = require('vsimoes.keymap').inoremap

nnoremap('<leader>w', ':w<CR>')
nnoremap('<leader>q', ':q<CR>')
nnoremap('<leader>pv', '<cmd>Ex<CR>')
inoremap('jk', '<esc>')

local telescope = require('telescope.builtin')

nnoremap('<leader>ff', telescope.find_files)
nnoremap('<leader>fg', telescope.live_grep)
nnoremap('<leader>fb', telescope.buffers)
nnoremap('<leader>fh', telescope.help_tags)

nnoremap('<leader>tt', ':TestNearest<CR>')
nnoremap('<leader>tT', ':TestFile<CR>')
nnoremap('<leader>ta', ':TestSuite<CR>')
nnoremap('<leader>tl', ':TestLast<CR>')
nnoremap('<leader>tg', ':TestVisit<CR>')
