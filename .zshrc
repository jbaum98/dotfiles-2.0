## History
HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=20000
setopt inc_append_history share_history

## Misc. Settings
# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle :compinstall filename '$HOME/.zshrc'

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

zplug "plugins/git", from:oh-my-zsh
zplug "plugins/ssh-agent", from:oh-my-zsh
zplug "lib/directories", from:oh-my-zsh

zplug "lib/clipboard", from:oh-my-zsh, if:"[[ $OSTYPE == *darwin* ]]"

zplug "zsh-users/zsh-syntax-highlighting", defer:3
zplug "zsh-users/zsh-history-substring-search", defer:3
zplug "zsh-users/zsh-autosuggestions", defer:3

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
source ~/.liquidprompt/liquidprompt	
