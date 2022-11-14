local nnoremap = require('vsimoes.keymap').nnoremap 

nnoremap('<leader>w', ':w<CR>')  
nnoremap('<leader>pv', '<cmd>Ex<CR>')  

local telescope = require('telescope.builtin')

nnoremap('<leader>ff', telescope.find_files)
nnoremap('<leader>fg', telescope.live_grep)
nnoremap('<leader>fb', telescope.buffers)
nnoremap('<leader>fh', telescope.help_tags)
