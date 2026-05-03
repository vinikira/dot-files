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
            vim.keymap.set("n", "<leader>tt", function()
                local file = vim.fn.expand("%:p")
                if file == "" then return end

                local alternate

                -- Lógica específica para Elixir (lib/ vs test/)
                if file:match("/lib/") then
                    -- De Implementação para Teste: troca /lib/ por /test/ e .ex por _test.exs
                    alternate = file:gsub("/lib/", "/test/"):gsub("%.ex$", "_test.exs")
                elseif file:match("/test/") then
                    -- De Teste para Implementação: troca /test/ por /lib/ e _test.exs por .ex
                    alternate = file:gsub("/test/", "/lib/"):gsub("_test%.exs$", ".ex")
                else
                    -- Fallback para outras linguagens (mesmo diretório)
                    local base, ext = file:match("^(.-)%.([^%.]+)$")
                    if not base then return end

                    local test_suffixes = { "_test", ".test", ".spec", "_spec" }
                    local is_test = false

                    for _, suffix in ipairs(test_suffixes) do
                        if base:match(suffix .. "$") then
                            alternate = base:sub(1, #base - #suffix) .. "." .. ext
                            is_test = true
                            break
                        end
                    end

                    if not is_test then
                        alternate = base .. "_test." .. ext
                    end
                end

                if vim.fn.filereadable(alternate) == 1 then
                    vim.cmd("edit " .. alternate)
                else
                    print("File not found" .. vim.fn.fnamemodify(alternate, ":."))
                end
            end, { desc = "Toggle Test/Implementation (Elixir & Generic)" })
        end,
    },
}
