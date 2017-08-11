import re
import statistics
import xml.etree.ElementTree as ET

# Pull out information of interest for each game from all XML files and print
# the information to a CSV file.

intre = re.compile('[0-9]+')

# Helper functions ------------------------------------------------------------

def strtoint(num):
    """Extract an integer value from a player/age number string."""
    intnum = None
    try:
        intnum = int(num)
    except ValueError:
        rematches = intre.search(num)
        if rematches:
            intnum = int(rematches.group(0))
        else:
            intnum = 0
    return intnum

def listtobest(players):
    """Calculate a sensible single value of best players."""
    if len(players) == 1:
        return float(players[0])
    elif max([players[i+1]-players[i] for i in range(len(players)-1)]) > 1:
        return float(players[0])
    else:
        return float(statistics.median(players))
    
def bestplayers(players, votes):
    """Find the highest-voted number of players for a game.

    In the event of ties, we have two approaches:
    If only adjacent numbers are tied, return their median.
    If non-adjacent numbers are tied, just return the fewest players rated
    "best."
    If there are no votes, return "NA".
    """
    if not (players and votes):
        return "NA"
    maxvotes = max(votes)
    maxindices = [index for index, val in enumerate(votes) if val == maxvotes]
    bplayers = [strtoint(players[index]) for index in maxindices]
    return listtobest(bplayers)

def bestage(ages, votes):
    """Find the highest-voted suggested minimum age for a game.

    If there are no votes, return "NA".
    """
    if not (ages and votes):
        return "NA"
    maxvotes = max(votes)
    maxindices = [index for index, val in enumerate(votes) if val == maxvotes]
    bestages = [strtoint(ages[index]) for index in maxindices]
    return float(statistics.median(bestages))

def valuelist(rt, elementname, numfunc):
    """Parse file for all matching elementnames and return values as list."""
    return [numfunc(val) if val else "NA" for val in
            [item.attrib["value"] for item in
             rt.findall(".//" + elementname)]]

def parsegamedata(filenum, outfile):
    """Open the XML file and parse pertinent information for each game.

    This is the main workhorse function. It creates lists for each type of
    data with elements for each game in the file, then prints the lists
    to a CSV file, one game per line.
    """
    root = None

    with open("xml/bggxml_" + str(filenum*100+1) + ".xml", 'r') as xmlfile:
        root = ET.parse(xmlfile).getroot()

    ## Extract the simple information for each game

    title = [item.attrib["value"] for item in
             root.findall(".//name[@type='primary']")]

    ## Watch out for games with missing info; replace with "NA" 
    
    year = valuelist(root, "yearpublished", int)

    minplayers = valuelist(root, "minplayers", int)

    maxplayers = valuelist(root, "maxplayers", int)

    minage = valuelist(root, "minage", int)

    playtime = valuelist(root, "playingtime", int)

    avgrating = valuelist(root, "average", float)

    geekrating = valuelist(root, "bayesaverage", float)

    avgweight = valuelist(root, "averageweight", float)

    ## A single game can have any number of categories and mechanics

    categories = ['|'.join(catlist) for catlist in [[category.attrib["value"]
                for category in categories] for categories in
                [game.findall(".//link[@type='boardgamecategory']")
                 for game in root.findall("*")]]]

    mechanics = ['|'.join(mechlist) for mechlist in [[mechanic.attrib["value"]
                for mechanic in mechanics] for mechanics in
                [game.findall(".//link[@type='boardgamemechanic']")
                 for game in root.findall("*")]]]

    ## Finding the highest-suggested number of players is more involved:

    numplaypolls = root.findall(".//poll[@name='suggested_numplayers']")
    numplaylists = [[result.attrib["numplayers"] for result in
                     poll.findall("./results")] for poll in numplaypolls]
    bestplaylists = [[int(best.attrib["numvotes"]) for best in
                      poll.findall("./results/result[@value='Best']")]
                     for poll in numplaypolls]

    suggestedplayers = [bestplayers(numplaylists[i], bestplaylists[i])
                    for i in range(len(numplaylists))]

    ## Suggested player age is also a poll for which we have to find the
    ## highest-voted value(s)

    agepolls = root.findall(".//poll[@name='suggested_playerage']")
    agelists = [[choice.attrib["value"] for choice in
                 poll.findall("./results/result")] for poll in agepolls]
    agevotelists = [[int(choice.attrib["numvotes"]) for choice
                     in poll.findall("./results/result")] for poll in agepolls]

    suggestedage = [bestage(agelists[i], agevotelists[i]) for i in
                    range(len(agelists))]

    ## Now print the data to a CSV file ---------------------------------------
    ## Escape " in titles with another "
    for i in range(len(title)):
        vals = ['\"' + title[i].replace('"', '""') + '\"',
                year[i], minplayers[i], maxplayers[i], minage[i],
                suggestedplayers[i], suggestedage[i], playtime[i],
                avgweight[i], avgrating[i], geekrating[i],
                '\"' + categories[i] + '\"', '\"' + mechanics[i] + '\"']
        outfile.write(", ".join([str(val) for val in vals]) + '\n')

# =============================================================================
# Open each XML file sequentially, extract the data, and print it to CSV

with open("bgg_games_20170415.csv", 'w') as csvout:
    csvout.write("Title, Year, MinPlayers, MaxPlayers, MinAge, " +
                 "SuggestedPlayers, SuggestedAge, PlayTime, AvgWeight, " +
                 "AvgRating, GeekRating, Categories, Mechanics\n")
    for i in range(2255):
        print(str(i))
        parsegamedata(i, csvout)
