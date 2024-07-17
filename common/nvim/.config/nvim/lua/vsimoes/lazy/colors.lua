return {
    { 
        "folke/tokyonight.nvim",
        lazy = false,
        opts = {},
        config = function()
            vim.cmd.colorscheme("tokyonight-moon")
        end
    }
}
