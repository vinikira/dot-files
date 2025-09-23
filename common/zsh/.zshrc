# Starship shell
eval "$(starship init zsh)"

# User configuration

# asdf vm
export ASDF_DATA_DIR=~/.asdf
export PATH=$ASDF_DATA_DIR/shims:/usr/local/bin:$PATH

# You may need to manually set your language environment
export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c -a emacs"
export VISUAL="emacsclient -c -a emacs"
export ELIXIR_EDITOR="emacsclient -n +__LINE__ __FILE__"

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

# Custom Aliases
alias start_minidlna='minidlnad -f $HOME/.config/minidlna/minidlna.conf -P $HOME/.config/minidlna/minidlna.pid'
alias clojurer='rlwrap clojure'
alias sbcl='rlwrap sbcl'
alias ec='emacsclient -cn -e "(progn (raise-frame) (x-focus-frame (selected-frame)))"'
alias et='emacsclient -t'
alias e='emacsclient -n'
alias ee='emacsclient --eval "(emacs-everywhere)"'
alias livebook_docker='docker run -p 8080:8080 livebook/livebook'
alias ls='ls --color'
