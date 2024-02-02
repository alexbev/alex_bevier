import urllib.request
import json
import ssl

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

uin = input('Enter URL:')
uhand = urllib.request.urlopen(uin).read().decode()
data = json.loads(uhand)
print(type(data))

count = 0
print('User Count:', len(data['comments']))
# print(data['comments'])
for user in data['comments']:
    x = (user['count'])
    count = count + x
print('Total Sum:', count)