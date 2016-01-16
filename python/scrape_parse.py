#######################################################################################
##  Filename: scrape_parse.py
##  Purpose: Scrape and parse apple store locations
##  Uses data from:
##  Assumes packages: urllib, BeautifulSoup
##  Output to data/applestores.txt
##  Last Edited: 16 January 2015
##  Christopher Boylan, Penn State University
#######################################################################################
## Load Packages
#######################################################################################
from BeautifulSoup import BeautifulSoup ## for parsing
import urllib ## to open url
#######################################################################################
## Get html
#######################################################################################
page = 'http://www.apple.com/retail/storelist/' ## url
html = urllib.urlopen(page).read() ## get html
#######################################################################################
## Parse store locations
#######################################################################################
## Soup the html page
soup = BeautifulSoup(html) 
## Empty list for stores
storelist =['city,street,state']
## Get US stores only
usstores = soup.find('div',{'id':'usstores'})
## Extract state names
states = usstores.findAll('h3')
states =[i.getText() for i in states]
## Extract city names
statestores= usstores.findAll('ul')
for i,s in zip(statestores,states):
    stores = i.findAll('li')
    stores = [k.getText() for k in stores]
    for k in stores:
        storelist.append(k+','+s)
## print stores
for i in storelist:
    print i
## Write file
out = open('data/applestores.txt', 'w')
for i in storelist:
  out.write("%s\n" % i)