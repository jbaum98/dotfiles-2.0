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


## Ensure emacs respects ~/.Xresources
if [ -e $HOME/.Xresources ]; then
    xrdb ~/.Xresources 
fi
