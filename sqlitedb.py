import sqlite3

conn = sqlite3.connect('mbox.sqlite')
cur = conn.cursor()

cur.execute('DROP TABLE IF EXISTS Counts')

cur.execute('''
CREATE TABLE Counts (org TEXT, count INTEGER)''')

fname = input('Enter file name: ')
if len(fname) < 1:
    fname = 'mbox-short.txt'
fh = open(fname)
for line in fh:
    if not line.startswith('From: '):
        continue
    pieces = line.split
    email = pieces[1]
    print(email)
        