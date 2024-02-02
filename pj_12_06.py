import socket
import urllib
from urllib.request import urlopen
from bs4 import BeautifulSoup
import ssl

# Ignore SSL certificate errors
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

url = input('Enter Server Name: ')
html = urllib.request.urlopen(url, context = ctx).read()
soup = BeautifulSoup(html, 'html.parser')
# print(soup)

s = 0
tags = soup('span')
for tag in tags:
    contents = int(tag.contents[0])
    s = s + contents
print(s)