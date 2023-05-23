require("tokyonight").setup({
    transparent = true, -- Enable this to disable setting the background color
    terminal_colors = true, -- Configure the colors used when opening a `:terminal` in Neovim
    transparent_sidebar = true,
})

vim.opt.background = 'dark'

vim.cmd('colorscheme tokyonight')
