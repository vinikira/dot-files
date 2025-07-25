# asdf vm
export ASDF_DATA_DIR=~/.asdf
# [ -f $HOME/.asdf/asdf.sh ] && source $HOME/.asdf/asdf.sh

# [ -f $HOME/.asdf/completions/asdf.bash ] && source $HOME/.asdf/completions/asdf.bash

# If you come from bash you might have to change your $PATH.
export PATH=$ASDF_DATA_DIR/shims:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Tmux
 # ZSH_TMUX_AUTOSTART="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git jsontools node npm tmux docker docker-compose brew pass emacs kubectl macos)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c -a emacs"
export VISUAL="emacsclient -c -a emacs"
export ELIXIR_EDITOR="emacsclient -n +__LINE__ __FILE__"

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Nix package manager
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi

# custom file
[ -f $HOME/.custom ] && source $HOME/.custom

# direnv
(( $+commands[direnv] )) && eval "$(direnv hook zsh)"

# vterm integration
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

find_file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}

# end vterm integration

# Erlang/Elixir flags
export ERL_AFLAGS="-kernel shell_history enabled"
export KERL_BUILD_DOCS=yes
export KERL_DOC_TARGETS=chunks

# Gnome cedilla
export GTK_IM_MODULE=cedilla

# tramp on macos
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='> '

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias start_minidlna='minidlnad -f $HOME/.config/minidlna/minidlna.conf -P $HOME/.config/minidlna/minidlna.pid'
alias clojurer='rlwrap clojure'
alias sbcl='rlwrap sbcl'
alias ec='emacsclient -cn -e "(progn (raise-frame) (x-focus-frame (selected-frame)))"'
alias et='emacsclient -t'
alias e='emacsclient -n'
alias ee='emacsclient --eval "(emacs-everywhere)"'
alias livebook_docker='docker run -p 8080:8080 livebook/livebook'
