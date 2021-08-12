#!/usr/bin/env bash

nitrogen --restore &

if [ -z "$(pgrep nm-applet)" ]; then
    nm-applet &
fi

if [ -z "$(pgrep blueman-applet)" ]; then
   blueman-applet &
fi

if [ -z "$(pgrep volumeicon)" ]; then
    volumeicon &
fi

if [ -z "$(pgrep trayer)" ]; then
    trayer --edge top --align right --widthtype request \
        --padding 6 --SetDockType true --SetPartialStrut true \
        --expand true --monitor 0 --transparent true --alpha 0 \
        --tint 0x282c34  --height 22 &
fi

if [ -z "$(pgrep crd)" ]; then
    crd --start &
fi

if [ -z "$(pgrep discord)" ]; then
    discord &
fi

if [ -z "$(pgrep spotify)" ]; then
    spotify &
fi

if [ -z "$(pgrep -x emacs)" ]; then
    emacs --daemon &
fi

# /home/trey/.config/conky/getAlbumCoverConky &
# conky &

if [ -z "$(pgrep flameshot)" ]; then
    flameshot &
fi

if [ -z "$(pgrep barrier)" ]; then
    barrier --config /home/trey/barrier/barrier.conf
fi

if [ -z "$(pgrep cadmus)" ]; then
    cadmus &
fi

if [ -z "$(pgrep steam)" ]; then
    steam &
if
