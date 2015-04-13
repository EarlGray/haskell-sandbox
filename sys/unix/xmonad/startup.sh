#!/bin/sh

[ "`pgrep dzen`" ] && exit 0

setxkbmap -layout "us,ua" -option "grp:alt_space_toggle,ctrl:nocaps"
. ~/.screenlayout/normal.sh

while true; do date; sleep 1; done | dzen2 -p 1 -h 24 -fg \#202020 -bg \#d8d8d8 &         #-fn '-bitstream-courier 10 pitch-medium-r-normal--0-0-0-0-m-0-adobe-standard' &
sleep 1
trayer --align right --edge top --widthtype request --height 24 --margin 24 --SetDockType true &
sleep 1
xxkb & kbdd &
kmix &
skype & yakuake & rsibreak &

