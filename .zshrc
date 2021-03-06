## History
HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=20000
setopt inc_append_history share_history

# fzf keybindings and completion
[[ -f ~/.nix-profile/share/fzf/key-bindings.zsh ]] && source ~/.nix-profile/share/fzf/key-bindings.zsh
[[ -f ~/.nix-profile/share/fzf/completion.zsh ]] && source ~/.nix-profile/share/fzf/completion.zsh

## Misc. Settings
# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle :compinstall filename '$HOME/.zshrc'
# Completion Makefile targets
zstyle ':completion:*:make:*:targets' call-command true

autoload -Uz compinit
compinit
# End of lines added by compinstall

setopt extended_glob
setopt interactivecomments
skip_global_compinit=1

autoload -U promptinit
promptinit
setopt prompt_subst

## Keybindings
bindkey '^[[1;3C' forward-word
bindkey '^[f' forward-word
bindkey '^[[1;3D' backward-word
bindkey '^[b' backward-word
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey '^[[1;5D' emacs-backward-word
bindkey '^[[1;5C' emacs-forward-word
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

## Packages
export ZPLUG_HOME=~/.zplug
source $ZPLUG_HOME/init.zsh

zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"

zplug "srijanshetty/zsh-pandoc-completion"

zplug "plugins/ssh-agent", from:oh-my-zsh
zplug "lib/directories", from:oh-my-zsh

zplug "lib/clipboard", from:oh-my-zsh, if:"[[ $OSTYPE == *darwin* ]]"

zplug "zsh-users/zsh-syntax-highlighting", defer:3
zplug "zsh-users/zsh-history-substring-search", defer:3
zplug "zsh-users/zsh-autosuggestions", defer:3

zplug "romkatv/powerlevel10k", use:powerlevel10k.zsh-theme

# Install plugins if there are plugins that have not been installed
if ! zplug check; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load

# Theme
# source ~/.liquidprompt/liquidprompt

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
