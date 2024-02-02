import socket
import urllib.request
from bs4 import BeautifulSoup
import ssl

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

url = input('Enter URL:')
html = urllib.request.urlopen(url).read()
soup = BeautifulSoup(html, 'html.parser')
tags = soup('a')
for tag in tags:
    # tag = tag.get('href')
    # print(tag)
    newlink = tags[17]
    nlink = newlink.get('href')
    # print(nlink)
print('Retrieving:', url)
print ('Retrieving:', nlink)

loop = 0
while loop < 6:
    html = urllib.request.urlopen(nlink).read()
    soup = BeautifulSoup(html, 'html.parser')
    tags = soup('a')
    newlink = tags[17]
    nlink = newlink.get('href')
    print('Retrieving:', nlink)
    loop = loop + 1