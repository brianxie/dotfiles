export EDITOR=vim

# Emacs-style keybindings
bindkey -e

# Prompt style
PWD='%~'
PROMPT_SYMBOL='❯'
JOBS='[%j]'

PROMPT="%B%F{blue}${PWD}%f%b %B${PROMPT_SYMBOL}%b "
RPROMPT="%F{green}${JOBS}%f"
