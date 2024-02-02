import sqlite3
import xml.etree.ElementTree as ET

conn = sqlite3.connect('tracksdb.sqlite')
cur = conn.cursor()

cur.executescript('''
DROP TABLE IF EXISTS Artist;
DROP TABLE IF EXISTS Album;
DROP TABLE IF EXISTS Track;
DROP TABLE IF EXISTS Genre;

CREATE TABLE Artist (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
    name TEXT UNIQUE
);
CREATE TABLE Album (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
    title TEXT UNIQUE,
    artist_id INTEGER
);
CREATE TABLE Track (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
    title TEXT UNIQUE,
    album_id INTEGER,
    genre_id INTEGER,
    len INTEGER,
    rating INTEGER,
    count INTEGER
);
CREATE TABLE Genre (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
    name TEXT
);
''')

fname = input('Enter File Name: ')
if (len(fname) < 1):
    fname = 'Library.xml'

def lookup(d, key):
    found = False
    for child in d:
        if found:
            return child.text
        if child.tag == 'key' and child.text == key:
            found = True
    return None

stuff = ET.parse(fname)
all = stuff.findall('dict/dict/dict')
print('dict count:', len(all))
for entry in all:
    # print(entry)
    if (lookup(entry, 'Track ID') is None):
        continue
    name = lookup(entry, 'Name')
    artist = lookup(entry, 'Artist')
    album = lookup(entry, 'Album')
    genre = lookup(entry, 'Genre')
    count = lookup(entry, 'Play Count')
    rating = lookup(entry, 'Rating')
    length = lookup(entry, 'Total Time')
    
    if name is None or artist is None or album is None or genre is None:
        continue
        
    print(name, artist, album, genre, count, rating, length)
    
    cur.execute('INSERT or IGNORE INTO Artist (name) VALUES (?)', (artist, ))
    cur.execute('SELECT id FROM Artist WHERE name = ?', (artist, ))
    artist_id = cur.fetchone()[0]
    
    cur.execute('INSERT or IGNORE INTO Album (title, artist_id) VALUES (?, ?)', (album, artist_id ))
    cur.execute('SELECT id FROM Album WHERE title = ?', (album, ))
    album_id = cur.fetchone()[0]
    
    cur.execute('INSERT or IGNORE INTO Genre (name) VALUES (?)', (genre, ))
    cur.execute('SELECT id FROM Genre WHERE name = ?', (genre, ))
    genre_id = cur.fetchone()[0]
    
    cur.execute('INSERT or REPLACE INTO Track (title, album_id, genre_id, len, rating, count) VALUES (?, ?, ?, ?, ?, ?)', (name, album_id, genre_id, length, rating, count ))
    
    conn.commit()
conn.close()

    
