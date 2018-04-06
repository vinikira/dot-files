#!/usr/bin/python
from subprocess import call
import imaplib

obj = imaplib.IMAP4_SSL('outlook.office365.com',993)
obj.login('your-email','your-password')
obj.select()
number = len(obj.search(None, 'UnSeen')[1][0].split())

if number > 0:
    call(['notify-send', 'You have a new e-mail message!'])

print(number)
