#######################################################################################
##  Filename: applestoremap.r
##  Purpose: Map apple store locations and county income data
##  Uses data from: 'applestores.txt',
##                  'est13ALLfixed.csv'
##                  'cb_2013_us_county_500k' 
##  Assumes packages: rgdal,dplyr,stringr,RCurl,XML,RColorBrewer
##  To install these packages run:
##  install.packages(c('rgdal','dplyr','stringr','RCurl','XML','RColorBrewer))
##  Output to figures/map.pdf
##  Last Edited: 16 January 2015
##  Christopher Boylan, Penn State University
#######################################################################################
## Load Packages and Prepare Directory
#######################################################################################
rm(list=ls()) ## remove objects from working environment
set.seed(1804) ## set seed
library(dplyr); library(stringr); library(rgdal)
library(RCurl); library(XML); library(RColorBrewer)
########################################################################################
## Geocoding function from R-bloggers
## http://www.r-bloggers.com/geocoding-r-and-the-rolling-stones-part-1/
########################################################################################
myGeo <- function(address="Atlanta,GA") {
  
  # Make sure we have the required libraries to do the work
  stopifnot(require(RCurl))
  stopifnot(require(XML))
  
  # Remove any spaces in the address field and replace with "+"
  address = gsub(" ","\\+",address)
  
  # Create the request URL according to Google specs
  google.url = "http://maps.googleapis.com/maps/api/geocode/xml?address="
  my.url = paste(google.url,address,"&sensor=false",sep="")
  Sys.sleep(0.5) # Just so we don't beat up the Google servers with rapid fire requests
  
  # Send the request to Google and turn it into an internal XML document
  txt = getURL(my.url)
  xml.report = xmlTreeParse(txt, useInternalNodes=TRUE)
  
  # Pull out the lat/lon pair and return it  
  place = getNodeSet(xml.report,  "//GeocodeResponse/result[1]/geometry/location[1]/*")
  lat.lon = as.numeric(sapply(place,xmlValue))
  names(lat.lon) = c("lat","lon")
  return(lat.lon)
}
########################################################################################
## Import apple data scraped from:
## http://www.apple.com/retail/storelist/
########################################################################################
## Import apple data
data <- read.csv('data/applestores.txt', stringsAsFactors = F)

## Get Latitude and Longitude from city names
df <- t(sapply(paste(data$street, data$city, data$state), myGeo))
colnames(df) <- c('lat','lon')
apple.geocoded <- data.frame(df)
########################################################################################
## Import 2013 county income/poverty data downloaded:
## http://www.census.gov/did/www/saipe/data/statecounty/data/2013.html
## I manually deleted the nuisance top rows in excel
########################################################################################
income <- read.csv('data/est13ALLfixed.csv', 
                   stringsAsFactors = F,
                   colClasses=c("State.FIPS.Code" = "character",
                                "County.FIPS.Code" = "character"))
## drop state and USA rows (i.e. those with County.FIPS.Code==0)
income <- filter(income, County.FIPS.Code != '000')
names(income)
## subset data
income <- select(income,State.FIPS.Code, County.FIPS.Code, Postal.Code, Name, 
                 Poverty.Percent..All.Ages, Median.Household.Income)
## convert povery and household income from char to num
income$poverty <-  as.numeric(income$Poverty.Percent..All.Ages)
income$medhouseincome <- as.numeric(str_replace(income$Median.Household.Income, ", ", ""))
## Create one fips variable by concatenating the state and country codes
income$fips <- paste(income$State.FIPS.Code, income$County.FIPS.Code,sep='')
########################################################################################
## Import 2013 county 500k shapefile downloaded from:
## https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
########################################################################################
county <- readOGR(dsn = "shapefile/", layer = "cb_2013_us_county_500k") ## read in map
county$fips <- paste(county$STATEFP, county$COUNTYFP, sep = '') ## 
choropleth <- merge(county, income, by = c("fips")) ## merge using fips indicator
########################################################################################
## Draw map:
#######################################################################################
## Set breaks for maps
choropleth_breaks <-(cut(choropleth$medhouseincome, 
                         breaks = c(seq(20000, 70000, by = 10000), 120000)))
palette(brewer.pal(6, "BuGn")) ## Blue Green palette

pdf('figures/map.pdf', width = 11,height = 7)
par(mar = c(1, 1, 1, 1) + 0.1)#set plot paramaters c(bottom, left, top, right) 
layout(matrix(1:2, nrow = 1), widths = c(0.95, 0.05)) ## Matrix to combine map and legend
plot(c(-125,-65), c(25,50), type = 'n', xlab = "", ylab = "",axes = F) ## Exclude Alaska, islands
plot(choropleth, col = (choropleth_breaks), add=TRUE, border = "black",lwd = 1) ## Draw map
points(c(apple.geocoded$lon), c(apple.geocoded$lat), col = 'black',
       bg = "lightpink3", pch = 21, cex = .85) ## Overlay store locations
## Set parameters for legend
xl <- 1
yb <- 1
xr <- 1.5
yt <- 2
## Set plot paramaters
par(mar = c(5.1, 0.5, 4.1, 0.5))
plot(NA, type="n",ann = F,xlim = c(1, 2), ylim=c(1, 2),xaxt = "n", yaxt = "n", bty = "n")
rect(
  xl,
  head(seq(yb, yt, (yt-yb)/6), -1),
  xr,
  tail(seq(yb, yt, (yt-yb)/6), -1),
  col = palette()
)
leglabel <- c('20-30', '30-40', '40-50', '50-60', '60-70', '70+') ## labels
mtext(leglabel, side = 2, at = tail(seq(yb, yt, (yt-yb)/6), -1)-0.05,
      las = 2,cex = 0.7) ## labels next to scale
mtext("Median Income\n($ thousands)", side = 3, at = c(1), font = 2, cex=0.7) ## title for legend
dev.off()
