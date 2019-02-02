## Environment variables
export EDITOR="$(command -v nvim >/dev/null && echo nvim || echo vim)"
export PAGER="less"
export LANG="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export TZ=:/etc/localtime
export TERM="xterm-256color"

## Run Nix hooks
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then 
  . $HOME/.nix-profile/etc/profile.d/nix.sh
fi

export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"

