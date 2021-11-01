#!/usr/bin/env bash

cd ~/.xmonad/
cd xmonad-git
git pull
cd ~/.xmonad/
cd xmonad-contrib-git
git pull
cd ~/.xmonad/
cd xmobar-git
git pull
stack install
