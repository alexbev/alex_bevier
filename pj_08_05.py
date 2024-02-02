fname = input('Enter File Name')
fhand = open(fname)
count = 0
for line in fhand:
    if not line.startswith('From '):
        continue
    else:
        words = line.split()
        ehand = words[1]
        print(ehand)
        count = count + 1
print('There were', count, 'lines in the file with From as the first word')