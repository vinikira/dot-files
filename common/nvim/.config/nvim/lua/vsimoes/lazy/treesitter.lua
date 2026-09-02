return {
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        lazy = false,
        init = function()
            local parsers = {
                "javascript",
                "typescript",
                "c",
                "lua",
                "rust",
                "jsdoc",
                "bash",
                "elixir",
                "heex",
                "eex",
                "ruby",
                "html",
            }

            local group = vim.api.nvim_create_augroup("VSimoesTreesitter", { clear = true })

            vim.api.nvim_create_autocmd({ "BufEnter", "FileType" }, {
                group = group,
                callback = function()
                    if vim.bo.buftype ~= "" then
                        return
                    end

                    pcall(vim.treesitter.start, 0)
                end,
            })

            vim.api.nvim_create_autocmd("User", {
                group = group,
                pattern = "VeryLazy",
                once = true,
                callback = function()
                    require("nvim-treesitter").install(parsers)
                end,
            })
        end,
    },
    {
        "nvim-treesitter/nvim-treesitter-textobjects",
        lazy = false,
        config = function()
            require("nvim-treesitter-textobjects").setup({
                select = {
                    enable = true,
                    lookahead = true,
                    keymaps = {
                        ["af"] = "@function.outer",
                        ["if"] = "@function.inner",
                    },
                },
            })
        end,
    },
}
