# zshrc - stefan@arentz.ca

bindkey -e							# Use emacs command line editing
bindkey ' ' magic-space				# Expand history items on space

bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward

setopt ignore_eof					# Don't exit shell on ^D
setopt auto_cd						# Change directories without using cd
setopt append_history				# Append history (for multiple sessions)
setopt hist_ignore_dups				# Ignore duplicate commands
setopt extended_history				# Save timestamps in history file
setopt no_beep						# No beeping. I hate beeping shells.
setopt complete_in_word				# Complete inside words

watch=(notme)						# Watch others

# Aliases

alias si='sudo -E zsh'

alias ls='ls -GF'
alias ll='ls -l'
alias la='ls -la'

alias -g m='more'
alias -g h='head'
alias -g t='tail'

# Suffix aliases for OS X

if [ `uname -s` = "Darwin" ]; then
  alias -s pdf=open					# open pdf files with preview
fi

alias em='emacs'
alias slime='emacs -f slime'
alias news='emacs -f gnus'
alias ta='tmux attach -d'

# History

HISTFILE=~/.zhistory				# History file
HISTSIZE=1000						# Number of lines rememberd
SAVEHIST=1000						# Number of lines stored in the file

# Allow history editing with $EDITOR
autoload -U edit-command-line		
zle -N edit-command-line
bindkey '\ee' edit-command-line


# Setup IRC

export IRCSERVER=irc.xs4all.nl
export IRCNAME="Stefan Arentz"
export IRCNICK=st3fan

# Setup paths from a bunch of known places

setup_paths() {
  if [ -d "$1/bin" ]; then
    export PATH="$1/bin:$PATH"
  fi
  if [ -d "$1/sbin" ]; then
    export PATH="$1/sbin:$PATH"
  fi
  if [ -d "$1/man" ]; then
    export MANPATH="$1/man:$MANPATH"
  fi
  if [ -d "$1/share/info" ]; then
    export INFOPATH="$1/share/info:$INFOPATH"
  fi
}

setup_paths "$HOME"
setup_paths /usr/local
setup_paths /opt/local
setup_paths /usr/local/mysql
setup_paths /usr/local/apache-maven
setup_paths /usr/local/apache-ant
setup_paths /usr/local/CrossPack-AVR
setup_paths /opt/local/Library/Frameworks/Python.framework/Versions/2.6

# Android SDK Stuff

if [ -d "$HOME/Google/android-sdk-macosx" ]; then
	export PATH="$PATH:$HOME/Google/android-sdk-macosx/tools"
	export PATH="$PATH:$HOME/Google/android-sdk-macosx/platform-tools"
fi

if [ -d "$HOME/Google/android-sdk-linux" ]; then
	export PATH="$PATH:$HOME/Google/android-sdk-linux/tools"
	export PATH="$PATH:$HOME/Google/android-sdk-linux/platform-tools"
fi

# For EC2

if [ -d /usr/local/ec2-api-tools ]; then
  export EC2_HOME=/usr/local/ec2-api-tools
  setup_paths /usr/local/ec2-api-tools
fi

if [ -d /usr/local/IAMCli ]; then
  export AWS_IAM_HOME=/usr/local/IAMCli
  setup_paths /usr/local/IAMCli
fi

if [ -x "`which ec2din`" ]; then
  alias ecc-show-instances=ec2din \
    | grep ^INSTANCE \
    | grep -v terminated \
    | awk '{ printf("%s %s %s %s\n", $2, $4, $5, $9) }'
fi

# Setup JAVA_HOME through OSX's java_home

if [ -x /usr/libexec/java_home ]; then
  export JAVA_HOME=$(/usr/libexec/java_home)
fi

# If we have less then use that as the pager

if [ -x "`which less`" ]; then
  export PAGER=less
  alias more='less'
fi

# If we have vim then use it as the default editor

if [ -x "`which vim`" ]; then
  export VISUAL=`which vim`
  export EDITOR=`which vim`
else
  export VISUAL=`which vi`
  export EDITOR=`which vi`
fi

# Setup completion

autoload -U compinit
compinit

zstyle '*' hosts pegasus.local galactica.local viper.local \
	home.local appletv.local satelefoon.local \
	82.94.255.141 keizer.soze.com \
	66.228.46.172 wopr.norad.org \
    kipdynamite.thedutchrepublic.com \
    hg.mozilla.org office.mozilla.org mpt-vpn.mozilla.com

# Setup the VCS Module

#autoload -Uz vcs_info
# 
#zstyle ':vcs_info:*' stagedstr '%F{28}●'
#zstyle ':vcs_info:*' unstagedstr '%F{11}●'
#zstyle ':vcs_info:*' check-for-changes true
#zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
#zstyle ':vcs_info:*' enable git svn hg
#precmd () {
#    zstyle ':vcs_info:*' formats ' [%F{green}%b%u%F{blue}]'
#    vcs_info
#}
 
# Set a prompt. Only show the hostname if we are not local.

if [ -n "$SSH_TTY" ]; then
  # If we are remote then we display the machine name
  PS1=$'%m %4 %{\e[31m%}%#%{\e[0m%} '
else
  PS1=$'%4 %{\e[31m%}%#%{\e[0m%} '
fi

if [ -x /usr/bin/sw_vers ]; then
  case `/usr/bin/sw_vers -productVersion` in
    ('10.7')
      # This is where Brew puts local Python modules
      export PYTHONPATH="/usr/local/lib/python2.7/site-packages/:$PYTHONPATH"
      ;;
    ('10.6')
      # This is where Brew puts local Python modules
      export PYTHONPATH="/usr/local/lib/python2.6/site-packages/:$PYTHONPATH"
      ;;
  esac
fi

# PostgreSQL

alias start-postgresql="postgres -D /usr/local/var/postgres"

# Source in local setup

if [ -f ".zshrc.local" ]; then
  source .zshrc.local
fi

