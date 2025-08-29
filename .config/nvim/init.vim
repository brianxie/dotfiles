" Loads vim configuration. See ':h nvim-from-vim'.
set runtimepath^=~/.config/vim runtimepath+=~/.config/vim/after
let &packpath = &runtimepath
source ~/.config/vim/vimrc

" Lua heredoc
lua << EOF
  vim.call('plug#begin')

  vim.fn['plug#']('nvim-treesitter/nvim-treesitter')
  vim.fn['plug#']('navarasu/onedark.nvim')

  vim.call('plug#end')

  require('nvim-treesitter.configs').setup({
    -- Use `ensure_installed = "all"` to include every language parser.
    ensure_installed = {
      'lua',
      'vim',
    },
    highlight = { enable = true },
  })

  require('onedark').load()
EOF
