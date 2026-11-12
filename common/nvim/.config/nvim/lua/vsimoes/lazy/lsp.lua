return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "saghen/blink.cmp",
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "j-hui/fidget.nvim",
    {
      "folke/lazydev.nvim",
      opts = {
        library = {
          { path = "${3rd}/luv/library", words = { "vim%.uv" } },
        },
      },
    },
  },

  config = function(_, opts)
    local lspconfig = require("lspconfig")

    require("fidget").setup({})
    require("mason").setup()
    require("mason-lspconfig").setup({
      handlers = {
        function(server_name)
          local config = {}
          config.capabilities = require("blink.cmp").get_lsp_capabilities()
          lspconfig[server_name].setup(config)
        end,
      },
    })

    vim.diagnostic.config({
      float = {
        focusable = false,
        style = "minimal",
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
      },
    })
  end,
}
