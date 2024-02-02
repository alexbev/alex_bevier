fname = input('Enter File Name:')
fhand = open(fname)
lst = list()
for line in fhand:
    line = line.rstrip()
    words = line.split()
    for word in words:
        if word in lst:
            continue
        else:
            lst.append(word)
lst.sort()
print(lst)