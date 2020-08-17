# Use Emacs-style keybindings
bindkey -e
# Immediately report changes in background jobs
setopt notify

## Completion system configuration
zstyle ':completion:*' completer _expand _complete _ignored _match
zstyle ':completion:*' matcher-list '' '+m:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' menu select=long-list select=0 # always use completion menu
zstyle ':completion:*' select-prompt '%l' # "n/N' row in completion menu
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
autoload -Uz compinit
compinit

# Environment variables and aliases
export EDITOR=vim

# Prompt style
PWD='%~'
PROMPT_SYMBOL='❯'
JOBS='[%j]'
PROMPT="%B%F{blue}${PWD}%f%b %B${PROMPT_SYMBOL}%b "
RPROMPT="%F{blue}${JOBS}%f"

# Syntax highlighting (package installed separately)
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
