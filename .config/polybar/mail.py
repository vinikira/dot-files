#!/usr/bin/python
from subprocess import call
import imaplib

obj = imaplib.IMAP4_SSL('outlook.office365.com',993)
obj.login('email','passs')
obj.select()
number = len(obj.search(None, 'UnSeen')[1][0].split())

counter_file = open('/tmp/email-counter', 'w+')
content_counter = counter_file.read()

if len(content_counter) == 0:
    content_counter = 0

current_count = int(content_counter)

if number > current_count:
    call(['notify-send', 'You have a new e-mail message!'])

counter_file.write(str(number))

counter_file.close()

print(str(number))
