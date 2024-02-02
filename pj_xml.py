import urllib.request
import xml.etree.ElementTree as ET
import ssl

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

uhand = input('Enter URL:')
url = urllib.request.urlopen(uhand)
# print(url)
data = url.read()

tree = ET.fromstring(data)
lst = tree.findall('comments/comment')
print('Tag Count:', len(lst))

count = 0
for item in lst:
    x = item.find('count').text
    indcount = int(x)
    count = count + indcount
print('Grand Total:', count)