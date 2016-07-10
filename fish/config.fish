# Do not show fish welcome message
set fish_greeting

if test -d ~/Go
  set -x GOPATH ~/Go
  set -x PATH $PATH $GOPATH/bin
end

if test -d /usr/local/CrossPack-AVR
  set -x PATH $PATH /usr/local/CrossPack-AVR/bin/
end

if test -d /System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/
  set -x PATH $PATH /System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/
end

if test -d /Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin
  echo "[*] Preferring latest swift toolchain"
  set -x PATH /Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin $PATH
end

# To make manpages more colorful
set -xU LESS_TERMCAP_mb (printf "\e[01;31m")      # begin blinking
set -xU LESS_TERMCAP_md (printf "\e[01;31m")      # begin bold
set -xU LESS_TERMCAP_me (printf "\e[0m")          # end mode
set -xU LESS_TERMCAP_se (printf "\e[0m")          # end standout-mode
set -xU LESS_TERMCAP_so (printf "\e[01;44;33m")   # begin standout-mode - info box
set -xU LESS_TERMCAP_ue (printf "\e[0m")          # end underline
set -xU LESS_TERMCAP_us (printf "\e[01;32m")      # begin underline

source ~/.config/fish/prompt.fish
source ~/.config/fish/ssh.fish
source ~/.config/fish/gpg.fish

