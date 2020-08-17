## General shell options
# Use Emacs-style keybindings
bindkey -e
# Immediately report changes in background jobs
setopt notify
# Display right-side prompt only on current command line
setopt transient_rprompt

# Enable menu completion (tab through ambiguous completions)
setopt auto_menu
# Display ambiguous completions in list (before menu completion)
setopt auto_list
# Display ambiguous completions in list immediately after unambiguous prefix completion
unsetopt list_ambiguous
# Don't immediately insert an ambiguous completion
unsetopt menu_complete

## Completion system configuration
zstyle ':completion:*' completer _expand _complete _ignored _match
zstyle ':completion:*' matcher-list '' '+m:{[:lower:][:upper:]}={[:upper:][:lower:]}'
# Always use menu selection (directional browsing) in completions
zstyle ':completion:*' menu select=long-list select=0
# Display 'n/N' row in menu selection for long lists
zstyle ':completion:*' select-prompt '%l'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
autoload -Uz compinit
compinit

## Environment variables and aliases
export EDITOR=vim

## Theme
PWD='%~'
PROMPT_SYMBOL='❯'
JOBS='[%j]'
PROMPT="%B%F{blue}${PWD}%f%b %B${PROMPT_SYMBOL}%b "
RPROMPT="%F{blue}${JOBS}%f"

## Packages
# Syntax highlighting (package installed separately)
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
