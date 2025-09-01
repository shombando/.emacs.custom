#!/usr/bin/sh
set -eu
git remote set-url --push origin --add git@codeberg.org:shom/.emacs.custom.git 
git remote set-url --push origin --add git@git.shom.dev:shom/.emacs.custom.git
git remote set-url --push origin --add git@git.sr.ht:~shom/.emacs.custom
git remote set-url --push origin --add git@github.com:shombando/.emacs.custom.git
