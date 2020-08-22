## General shell options
# Use Emacs-style keybindings
bindkey -e
# Immediately report changes in background jobs
setopt notify
# Display right-side prompt only on current command line
setopt transient_rprompt

# Don't immediately insert an ambiguous completion
unsetopt menu_complete
# Display ambiguous completions in list (before menu completion)
setopt auto_list
# Display list immediately after unambiguous prefix completion (without requiring an additional <TAB>)
unsetopt list_ambiguous
# Enable menu completion (tab through ambiguous completions)
setopt auto_menu

## Completion system configuration
# List of configured completer functions, order-sensitive
# Other completers can also be added (e.g. _correct, _approximate, or a custom function)
zstyle ':completion:*' completer _expand _complete _ignored _match
zstyle ':completion:*' matcher-list '' '+m:{[:lower:][:upper:]}={[:upper:][:lower:]}'
# Always use menu selection (cursor browsing) in completions
# Note that menu selection is different from menu completion
zstyle ':completion:*' menu select=0
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
JOBS='%j'
PROMPT="%B%F{blue}${PWD}%f%b %B${PROMPT_SYMBOL}%b "
RPROMPT="[%F{blue}${JOBS}%f]"

## Packages
# Syntax highlighting (package installed separately)
# Location may differ between systems
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

## Appendix: a note on completion behavior

# The following completion behavior is configured:
#   <TAB-1> inserts the longest unambiguous prefix (if applicable) and lists valid completions
#   <TAB-2> enters menu completion with menu selection

# If only a single completion is valid, <TAB-1> inserts that completion without listing.
# Note that glob expansion does not adhere to this behavior and always enters menu completion immediately.
