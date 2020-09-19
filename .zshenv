# Use '~/.config/zsh' as the base directory for other zsh dotfiles.
# zsh doesn't respect $XDG_CONFIG_HOME (since zsh is what sets the environment
# variables to begin with).
# However, subsequent config files respect $ZDOTDIR. The value is set here,
# since .zshenv is the first config file sourced by zsh.
export ZDOTDIR="$HOME/.config/zsh"

# Use '~/.config' as the base directory for configuration files, for programs
# which respect the XDG Base Directory Specification.
export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR='nvim'
