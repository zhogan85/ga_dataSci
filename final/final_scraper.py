from lxml.html import parse
from urllib2 import urlopen
import pandas as pd
from pandas.io.parsers import TextParser


def _unpack(row, kind='td'):
	elts = row.findall('.//%s' % kind)
	return [val.text_content() for val in elts]

def parse_nfl_data(table):
	rows = table.findall('.//tr')
	data = [_unpack(r) for r in rows[1:]]
	return data

# creates the data frame	
def nfl_year_data(urls):
	i = 0
	data = []
	head = ("rank", "player", "team", "pos", "att", "att/game", "yards", "avg", "yards/game", "TD", "long", "1st", "1st%", "20+", "40+", "fumbles")
	while i < len(urls):
		parsed = parse(urlopen(urls[i]))
		doc = parsed.getroot()
		tables = doc.findall('.//table')
		data += parse_nfl_data(tables[0])
		i += 1
	return TextParser(data, names=head).get_chunk()

# strips the escape characters from the data
def clean(data):
	dirty = ["player", "team", "yards", "long", "avg", "1st%"]
	i = 0
	while i < len(dirty):
		data[dirty[i]] = data[dirty[i]].str.replace('\t', '')
		data[dirty[i]] = data[dirty[i]].str.replace('\n', '')
		i += 1
	data["yards"] = data["yards"].str.replace(',', '')
	data["long"] = data["long"].str.replace('T', '')
	return data

# used to create separate data frames of just player and ranks,
# to be merged with the full season data frame from the prior season
def year_ranks(df):
	remove_head = ["team", "pos", "att", "att/game", "yards", "avg", "yards/game", "TD", "long", "1st", "1st%", "20+", "40+", "fumbles"]
	i = 0
	while i < len(remove_head):
		del df[remove_head[i]]
		i += 1
	return df

def year_yards(df):
	remove_head = ["rank", "team", "pos", "att", "att/game", "avg", "yards/game", "TD", "long", "1st", "1st%", "20+", "40+", "fumbles"]
	i = 0
	while i < len(remove_head):
		del df[remove_head[i]]
		i += 1
	return df
    

# arrays of the links for the complete regular season stats for running backs
nfl_urls_2012 = ["http://www.nfl.com/stats/categorystats?tabSeq=0&statisticCategory=RUSHING&conference=null&season=2012&seasonType=REG&d-447263-s=RUSHING_YARDS&d-447263-o=2&d-447263-n=1","http://www.nfl.com/stats/categorystats?tabSeq=0&season=2012&seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&statisticCategory=RUSHING&conference=null&d-447263-s=RUSHING_YARDS","http://www.nfl.com/stats/categorystats?tabSeq=0&season=2012&seasonType=REG&d-447263-n=1&d-447263-o=2&conference=null&statisticCategory=RUSHING&d-447263-p=3&d-447263-s=RUSHING_YARDS","http://www.nfl.com/stats/categorystats?tabSeq=0&season=2012&seasonType=REG&d-447263-n=1&d-447263-o=2&conference=null&statisticCategory=RUSHING&d-447263-p=4&d-447263-s=RUSHING_YARDS","http://www.nfl.com/stats/categorystats?tabSeq=0&season=2012&seasonType=REG&d-447263-n=1&d-447263-o=2&conference=null&statisticCategory=RUSHING&d-447263-p=5&d-447263-s=RUSHING_YARDS","http://www.nfl.com/stats/categorystats?tabSeq=0&season=2012&seasonType=REG&d-447263-n=1&d-447263-o=2&statisticCategory=RUSHING&conference=null&d-447263-p=6&d-447263-s=RUSHING_YARDS","http://www.nfl.com/stats/categorystats?tabSeq=0&season=2012&seasonType=REG&d-447263-n=1&d-447263-o=2&conference=null&statisticCategory=RUSHING&d-447263-p=7&d-447263-s=RUSHING_YARDS"]
nfl_urls_2011 = ["http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2011&experience=&Submit=Go&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2011&experience=&Submit=Go&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=3&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2011&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=4&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2011&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=5&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2011&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=6&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2011&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=7&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2011&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false"]
nfl_urls_2010 = ["http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2010&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2010&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=3&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2010&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=4&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2010&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=5&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2010&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=6&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2010&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=7&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2010&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false"]
nfl_urls_2009 = ["http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2009&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2009&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=3&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2009&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=4&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2009&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=5&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2009&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=6&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2009&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=7&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2009&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false"]
nfl_urls_2008 = ["http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2008&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2008&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=3&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2008&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=4&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2008&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=5&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2008&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=6&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2008&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=7&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2008&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false"]
nfl_urls_2007 = ["http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2007&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2007&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=3&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2007&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=4&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2007&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=5&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2007&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=6&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2007&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=7&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2007&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false"]
nfl_urls_2006 = ["http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2006&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2006&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=3&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2006&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=4&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2006&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=5&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2006&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=6&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2006&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=7&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2006&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false"]
nfl_urls_2005 = ["http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2005&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2005&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=3&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2005&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=4&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2005&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=5&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2005&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=6&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2005&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=7&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2005&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false"]
nfl_urls_2004 = ["http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2004&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2004&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=3&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2004&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=4&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2004&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=5&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2004&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=6&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2004&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=7&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2004&Submit=Go&experience=&archive=true&statisticCategory=RUSHING&conference=null&qualified=false"]
nfl_urls_2003 = ["http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2003&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=2&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2003&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=3&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2003&Submit=Go&experience=&archive=true&conference=null&statisticCategory=RUSHING&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=4&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2003&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=5&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2003&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=6&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2003&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false","http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=7&d-447263-s=RUSHING_YARDS&tabSeq=0&season=2003&experience=&Submit=Go&archive=true&statisticCategory=RUSHING&conference=null&qualified=false"]

# creates the 2012 full data frame
nflTable_2012 = nfl_year_data(nfl_urls_2012)
clean(nflTable_2012)
ranks_2012 = nfl_year_data(nfl_urls_2012)
clean(ranks_2012)
yards_2012 = nfl_year_data(nfl_urls_2012)
clean(yards_2012)
#2011
nflTable_2011 = nfl_year_data(nfl_urls_2011)
clean(nflTable_2011)
ranks_2011 = nfl_year_data(nfl_urls_2011)
clean(ranks_2011)
yards_2011 = nfl_year_data(nfl_urls_2011)
clean(yards_2011)
#2010
nflTable_2010 = nfl_year_data(nfl_urls_2010)
clean(nflTable_2010)
ranks_2010 = nfl_year_data(nfl_urls_2010)
clean(ranks_2010)
yards_2010 = nfl_year_data(nfl_urls_2010)
clean(yards_2010)
#2009
nflTable_2009 = nfl_year_data(nfl_urls_2009)
clean(nflTable_2009)
ranks_2009 = nfl_year_data(nfl_urls_2009)
clean(ranks_2009)
yards_2009 = nfl_year_data(nfl_urls_2009)
clean(yards_2009)
#2008
nflTable_2008 = nfl_year_data(nfl_urls_2008)
clean(nflTable_2008)
ranks_2008 = nfl_year_data(nfl_urls_2008)
clean(ranks_2008)
yards_2008 = nfl_year_data(nfl_urls_2008)
clean(yards_2008)
#2007
nflTable_2007 = nfl_year_data(nfl_urls_2007)
clean(nflTable_2007)
ranks_2007 = nfl_year_data(nfl_urls_2007)
clean(ranks_2007)
yards_2007 = nfl_year_data(nfl_urls_2007)
clean(yards_2007)
#2006
nflTable_2006 = nfl_year_data(nfl_urls_2006)
clean(nflTable_2006)
ranks_2006 = nfl_year_data(nfl_urls_2006)
clean(ranks_2006)
yards_2006 = nfl_year_data(nfl_urls_2006)
clean(yards_2006)
#2005
nflTable_2005 = nfl_year_data(nfl_urls_2005)
clean(nflTable_2005)
ranks_2005 = nfl_year_data(nfl_urls_2005)
clean(ranks_2005)
yards_2005 = nfl_year_data(nfl_urls_2005)
clean(yards_2005)
#2004
nflTable_2004 = nfl_year_data(nfl_urls_2004)
clean(nflTable_2004)
ranks_2004 = nfl_year_data(nfl_urls_2004)
clean(ranks_2004)
yards_2004 = nfl_year_data(nfl_urls_2004)
clean(yards_2004)
#2003
nflTable_2003 = nfl_year_data(nfl_urls_2003)
clean(nflTable_2003)
ranks_2003 = nfl_year_data(nfl_urls_2003)
clean(ranks_2003)
yards_2003 = nfl_year_data(nfl_urls_2003)
clean(yards_2003)




# Create the ranks data frame for 2012, to be merged with 2011 full data fram
ranks_2012 = year_ranks(ranks_2012)
yards_2012 = year_yards(yards_2012)

# Change the column header
ranks_2012.columns = ["rank_2012", "player"] 
yards_2012.columns = ["player", "yards_2012"]

# adds the a column with the following season's rank to data frame
nflTable_2011 = pd.merge(nflTable_2011, ranks_2012, how="left", on="player", left_on=None, right_on=None, left_index=False, right_index=False, sort=False)
nflTable_2011 = pd.merge(nflTable_2011, yards_2012, how="left", on="player", left_on=None, right_on=None, left_index=False, right_index=False, sort=False)

# Create the ranks data frame for 2011, to be merged with 2010 full data fram
ranks_2011 = year_ranks(ranks_2011)
yards_2011 = year_yards(yards_2011)

# Change the column header
ranks_2011.columns = ["rank_2011", "player"] 
yards_2011.columns = ["player", "yards_2011"]

# adds the a column with the following season's rank to data frame
nflTable_2010 = pd.merge(nflTable_2010, ranks_2011, how="left", on="player", left_on=None, right_on=None, left_index=False, right_index=False, sort=False)
nflTable_2010 = pd.merge(nflTable_2010, yards_2011, how="left", on="player", left_on=None, right_on=None, left_index=False, right_index=False, sort=False)

# Add a column to indicate year
# Will only do this once I can figure out how to do the linear
# regressions for multiple seasons at once
# nfl_table_2012["year"] = 2012

# To export a data frame to csv, do the following
#Training set
nflTable_2011.to_csv("train11.csv", sep=",", index=False)
nflTable_2010.to_csv("train10.csv", sep=",", index=False)

# Test set
nflTable_2012.to_csv("test.csv", sep=",", index=False)

# Good blog for multi variable linear regression, http://aimotion.blogspot.com/2011/10/machine-learning-with-python-linear.html
# and for simple linear regression, http://glowingpython.blogspot.com/2012/03/linear-regression-with-numpy.html

# TODO
# Create new data frames for years with ranks
# Merge new rank data frames with stats data frames,
# so we have a rank to test against
#
# Get new links, sorted by yards
# Drop columns link: http://stackoverflow.com/questions/13411544/delete-column-from-pandas-dataframe
# Merge data frames link: http://pandas.pydata.org/pandas-docs/stable/merging.html
#
#


