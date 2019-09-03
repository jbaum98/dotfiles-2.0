## Run Nix hooks
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
  . $HOME/.nix-profile/etc/profile.d/nix.sh
fi

## Environment variables
export EDITOR="$(command -v nvim >/dev/null && echo nvim || echo vim)"
export PAGER="less"
export LANG="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export TZ=:/etc/localtime
export TERM="xterm-256color"

## Aliases
if ls --color 2>/dev/null; then
    alias ls="ls --color"
fi
alias dfg="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"

## FZF configuration
export FZF_DEFAULT_COMMAND="fd . $HOME"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd -t d . $HOME"

## Ensure emacs respects ~/.Xresources
if command -v xrdb && [ -e $HOME/.Xresources ]; then
    xrdb $HOME/.Xresources
fi
