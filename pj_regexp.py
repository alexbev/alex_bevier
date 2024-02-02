import re
fname = input('Enter File Name: ')
fhand = open(fname)
s = 0
# print(type(s))
for line in fhand:
    y = re.findall('[0-9]+', line)
    for string in y:
        if string == '':
            continue
        else:
            x = int(string)
            s = s + x
print(s)