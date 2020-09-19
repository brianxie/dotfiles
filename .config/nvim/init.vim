" Loads vim configuration. See ':h nvim-from-vim'.
set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vim/vimrc
