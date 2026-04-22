" Loads vim configuration. See ':h nvim-from-vim'.
set runtimepath^=~/.config/vim runtimepath+=~/.config/vim/after
let &packpath = &runtimepath
source ~/.config/vim/vimrc

" Lua heredoc
lua << EOF
  vim.pack.add({
    "https://github.com/navarasu/onedark.nvim",
    "https://github.com/nvim-treesitter/nvim-treesitter",
  })

  require('nvim-treesitter').install {
    'lua',
    'vim',
  }
  vim.api.nvim_create_autocmd('FileType', {
    callback = function()
      pcall(vim.treesitter.start)
    end,
  })

  require('onedark').setup {
    style = 'darker',
  }
  require('onedark').load()
EOF
