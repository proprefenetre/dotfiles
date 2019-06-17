#! /usr/bin/env bash

# xcode
xcode-select --install

# homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew update

brew tap homebrew/cask
brew tap homebrew/cask-versions

casks=(firefox-developer-edition emacs)
packages=(bash fish vim git python imagemagick coreutils ffmpeg htop gnupg
          jqtmux tree docker nmap bash-completion fzf ghostscript ripgrep ipython
          jupyter pass bc tmux)

for cask in ${casks[@]}; do
    brew cask install $cask
done

for pkg in ${packages[@]}; do
    brew install $pkg
done

brew doctor
