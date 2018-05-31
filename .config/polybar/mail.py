#!/usr/bin/python
from subprocess import call
import imaplib
from os import path

file_path = '/tmp/email-counter'
obj = imaplib.IMAP4_SSL('outlook.office365.com',993)
obj.login('vinicius.simoes@runsmart.cloud','runsmart@2018')
obj.select()
number = len(obj.search(None, 'UnSeen')[1][0].split())

if not path.isfile(file_path) :
    temp_file = open(file_path, 'w')
    temp_file.write('0')
    temp_file.close()

temp_file = open(file_path, 'r')
content_counter = temp_file.read()
temp_file.close()

current_count = int(content_counter)

if number > current_count:
    call(['notify-send', 'You have a new e-mail message!'])

temp_file = open(file_path, 'w')
temp_file.write(str(number))
temp_file.close()

print(str(number))
