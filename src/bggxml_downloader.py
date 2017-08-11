import urllib.request

# First get all of the XML files
#
# We will go 100 "things" at a time, where not all "things" are
# necessarily board games
#
# type=boardgame in the URL will exclude non-boardgame items from being
# retrieved
#
# There are somewhere between 220000 and 230000 "things" in the database

for start in range(1,230000,100):
    xmlurl = ("https://www.boardgamegeek.com/xmlapi2/thing?" + "id=" +
              ','.join([str(i) for i in range(start,start+99)]) +
              "&type=boardgame&stats=1")
    urllib.request.urlretrieve(xmlurl, "xml/bggxml_" + str(start) + ".xml")
