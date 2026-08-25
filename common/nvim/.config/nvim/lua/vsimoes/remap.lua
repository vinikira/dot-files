vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex, { desc = "File explorer" })
vim.keymap.set('n', '<leader>w', ':w<CR>', { desc = "Write the file" })
vim.keymap.set('n', '<leader>q', ':q<CR>', { desc = "Quit" })
vim.keymap.set('i', 'jk', '<esc>', { desc = "Esc" })
vim.keymap.set('v', 'K', ':m \'<-2<CR>gv=gv', { desc = "Move line up" })
vim.keymap.set('v', 'J', ':m \'>+1<CR>gv=gv', { desc = "Move line down" })
vim.keymap.set("n", "J", "mzJ`z", { desc = "Join lines" })
vim.keymap.set('n', '<leader>y', '"+y', { desc = "Copy system clipboard" })
vim.keymap.set('v', '<leader>y', '"+y', { desc = "Copy system clipboard" })
vim.keymap.set('n', '<leader>Y', '"+Y', { desc = "Copy system clipboard" })
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
    { desc = "Replace word at point" })
vim.keymap.set("n", "<leader><leader>", function()
    vim.cmd("so %")
end, { desc = "Eval current lua file" })

vim.keymap.set("n", "<leader>ci", function()
    local command = vim.fn.input("Compile: ")
    if command ~= "" then
        vim.opt.makeprg = ""
        vim.cmd("make " .. command)
        vim.cmd("copen")
    end
end, { desc = "Quickfix interactive" })
vim.keymap.set("n", "<leader>co", "<cmd>copen<CR>", { desc = "Quickfix open" })
vim.keymap.set("n", "<leader>cc", "<cmd>cclose<CR>", { desc = "Quickfix close" })
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz", { desc = "Quickfix prev" })
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz", { desc = "Location list next" })
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz", { desc = "Location list prev" })
