return {
    {
        "nvim-neotest/neotest",
        dependencies = {
            "nvim-neotest/nvim-nio",
            "nvim-lua/plenary.nvim",
            "antoinemadec/FixCursorHold.nvim",
            "nvim-treesitter/nvim-treesitter",
            "nvim-neotest/neotest-plenary",
            "jfpedroza/neotest-elixir",
            "lawrence-laz/neotest-zig",
            "olimorris/neotest-rspec",
            "nvim-neotest/neotest-jest",
            "nvim-neotest/neotest-go",
        },
        config = function()
            local neotest = require("neotest")
            neotest.setup({
                adapters = {
                    require("neotest-plenary").setup({ }),
                    require("neotest-elixir"),
                    require("neotest-zig"),
                    require("neotest-rspec"),
                    require('neotest-jest'),
                    require("neotest-go"),
                }
            })

            vim.keymap.set("n", "<leader>tc", function()
                neotest.run.run()
            end)
            vim.keymap.set("n", "<leader>tf", function()
                neotest.run.run(vim.fn.expand("%"))
            end)
        end,
    },
}
