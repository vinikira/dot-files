return {
    "stevearc/conform.nvim",
    opts = {},
    config = function()
        require("conform").setup({
            format_on_save = {
                timeout_ms = 5000,
                lsp_format = "fallback",
            },
            formatters_by_ft = {
                c = { "clang-format" },
                cpp = { "clang-format" },
                lua = { "stylua" },
                go = { "gofmt" },
                odin = { "odinfmt" },
                javascript = { "biome" },
                typescript = { "biome" },
                json = { "biome" },
                css = { "biome" },
                html = { "biome" },
                yaml = { "biome" },
                markdown = { "biome" },
                graphql = { "biome" },
                elixir = { "mix" },
            },
            formatters = {
                ["clang-format"] = {
                    prepend_args = { "-style=file", "-fallback-style=LLVM" },
                },
                biome = {
                    append_args = { "--write" },
                },
            },
        })

        vim.keymap.set("n", "<leader>vf", function()
            require("conform").format({ bufnr = 0 })
        end, { desc = "Format buffer (conform)" })
    end,
}
