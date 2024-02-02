fname = input('Enter File Name:')
fhand = open(fname)
count = 0
tot = 0
for line in fhand:
    if not line.startswith('X-DSPAM-Confidence:'):
        continue
    else:
        count = count + 1
        value = line.find('0')
        v = line[value: ]
        tot = tot + float(v)
total = tot / count
print('Average spam confidence:', total)