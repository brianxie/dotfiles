export EDITOR=vim

# Emacs-style keybindings
bindkey -e

# Completion
autoload -Uz compinit
compinit

# Prompt style
PWD='%~'
PROMPT_SYMBOL='❯'
JOBS='[%j]'
PROMPT="%B%F{blue}${PWD}%f%b %B${PROMPT_SYMBOL}%b "
RPROMPT="%F{yellow}${JOBS}%f"

# Syntax highlighting (package installed separately)
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
