
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
# End of lines added by compinstall
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}" 
zstyle ':completion:*' menu select
autoload -Uz compinit
compinit

# imports
source ~/projects/zsh-git-prompt/zshrc.sh

autoload -Uz colors && colors
PS1='%{$bg[green]%}%n@%m%{$reset_color%}%{$fg[green]%} %1~ %{$reset_color%}$(git_super_status) %# '

setopt NO_HUP
setopt NO_CHECK_JOBS 


# Path

PATH=$PATH:~/.cabal/bin

#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
alias emacs='emacs -nw'
alias ls='ls -lh --color=auto'
alias ll='ls -a'
alias srg='sr google'
alias srw='sr wikipedia'
alias rtv='export BROWSER=linkopen; export EDITOR=vim; export PAGER=less;rtv'
alias vimread='vim -RNu ~/.vimreadrc' 
alias randomYoutube='mpv $(shuf /var/tmp/youtubeVideos)'
alias .='cd ..'
alias ..='cd ../..'
alias ...='cd ../../..'
alias ....='cd ../../../..'
alias poweroff='closeAllWindows; poweroff'
alias reboot='closeAllWindows; reboot'
alias slock='killall unclutter; slock; unclutter -grab &' #with unclutter, slock dont work
alias trans='rlwrap trans' # to use history in tranlation shell
alias jupyter='jupyter notebook ~/Dokumente/Uni/angewandteStatistik'
export VISUAL='vim'
~/scripts/nextApts #show all next Apts in the next 24 hours
export BROWSER=linkopen
# play the youtube search list
function mm() {
    mpv ytdl://ytsearch10:"$@"
}
function mma() {
    mpv --no-video ytdl://ytsearch10:"$@"
}
