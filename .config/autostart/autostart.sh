#!/usr/bin/env bash

picom --no-fading-openclose --fade-delta-time 0 &
nitrogen --restore &
nm-applet &
blueman-applet &
volumeicon &
trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x282c34  --height 22 &
crd --start &
discord &
spotify &
emacs --daemon &
/home/trey/.config/conky/getAlbumCoverConky &
conky &
flameshot &
lbry --hidden &
