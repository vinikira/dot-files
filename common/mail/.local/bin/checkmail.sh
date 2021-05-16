#!/bin/sh

STATE=`nmcli networking connectivity`

if [ $STATE = 'full' ]
then
    mbsync -CVa
    notmuch new
    notify-send -a emacsclient -i emacs \
                -i /usr/share/icons/Adwaita/64x64/status/mail-unread-symbolic.symbolic.png \
                "You have $(notmuch count tag:unread) unread emails!"
    exit 0
fi

echo "No internet connection."
exit 0
