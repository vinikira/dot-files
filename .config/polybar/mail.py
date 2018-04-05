#!/usr/bin/python

import imaplib
obj = imaplib.IMAP4_SSL('outlook.office365.com',993)
obj.login('your-email','your-password')
obj.select()
number = len(obj.search(None, 'UnSeen')[1][0].split())
if number>0:
    print(number)
else:
    print('')
