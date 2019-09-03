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
if command -v fd >/dev/null; then
    # Use fd (https://github.com/sharkdp/fd) instead of the default find
    # command for listing path candidates.
    # - The first argument to the function ($1) is the base path to start traversal
    # - See the source code (completion.{bash,zsh}) for the details.
    _fzf_compgen_path() {
        fd --hidden --follow --exclude ".git" --exclude ".Trash" . "$1"
    }

    # Use fd to generate the list for directory completion
    _fzf_compgen_dir() {
        fd --type d --hidden --follow --exclude ".git" --exclude ".Trash" . "$1"
    }

    export FZF_DEFAULT_COMMAND="fd . $HOME -H -E Library"
    export FZF_DEFAULT_OPTS=""
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND="fd -t d . $HOME"
fi

## Ensure emacs respects ~/.Xresources
if command -v xrdb && [ -e $HOME/.Xresources ]; then
    xrdb $HOME/.Xresources
fi

## Load bashrc if on Terminal.app and bash
if [ "$TERM_PROGRAM" = "Apple_Terminal" -a -n "$BASH_VERSION" ]; then
    source ~/.bashrc
fi
