#!/bin/sh

STATE=`nmcli networking connectivity`

if [ $STATE = 'full' ]
then
    mbsync -CVa
    notmuch new

    COUNT_MAIL=`notmuch count tag:unread`

    if [ $COUNT_MAIL != '0' ]
    then
        notify-send -a notmuch-emacs-client-mua \
                    -i emblem-mail \
                    "You have $COUNT_MAIL unread emails!"
    fi

    exit 0
fi

echo "No internet connection."
exit 0
