## Environment variables
export EDITOR="$(command -v nvim && echo nvim || echo vim)"
export PAGER="less"
export LANG="en_US.UTF-8"
export TZ='America/New_York'
# export CLASSPATH=".:/opt/algs4.jar"

## Run Nix hooks
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then 
  . $HOME/.nix-profile/etc/profile.d/nix.sh
fi
