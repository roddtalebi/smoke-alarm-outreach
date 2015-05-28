#
# BGAnalysis2.R
#
#===================
#
# To apply our model to New Orleans block groups, we first read in a shapefile of the geography, as well as American Community Survey (ACS) data for our independent variables.
# Data can be accessed from the Census Bureau's American Factfinder at http://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t).
# The following tables were used for New Orleans block groups:
#  * Table B25034 for Year Structure Built
#  * Table C17002 for Ratio of Income to the Poverty Level
#  * Table B25038 for Year Householder Moved into Unit
#
#===================
#
#


setwd("YOUR_WORKING_DIRECTORY")
packages = "YOUR_PACKAGE_DIRECTORY"
.libPaths(packages)
load("weights.RData")

library(maps)
library(maptools)
library(sp)
library(rgdal)

NOLA.proj <- CRS("+proj=lcc +lat_1=29.3 +lat_2=30.7 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=999999.9999999999 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")

# readOGR() is a function in the rgdal package that reads in shapefiles as R Spatial objects
# spTransform is used to transform Spatial objects from one coordinate system to another. The block groups
#  were originally in a lat/long projection, but for local data I usually prefer to use the Louisiana South Stateplane projection, which is defined as NOLA.proj in the code."
# ??so do we not need NOLA.proj? and then can get rid of the spTransform line???
# to get block groups shapefiles -> https://www.census.gov/cgi-bin/geo/shapefiles2010/main
BG <- readOGR(dsn=getwd(),layer="orl_census2010_blockgrp_pl")
# for readOGR -> the data source name (dsn= argument) is the folder (directory) where the shapefile is, and the layer is the name of the shapefile (without the .shp extension)
# ^conti... getwd() returns an absolute filepath representing the current working directory...points to working directory
BG <- spTransform(BG, CRS=NOLA.proj)
s.ages <- read.csv("ACS_13_5YR_B25034_with_ann.csv",skip=1)
# ^Year Structure Built
pov <- read.csv("ACS_13_5YR_C17002_with_ann.csv",skip=1)
# ^Ratio of Income to the Poverty Level
house.time <- read.csv("ACS_13_5YR_B25038_with_ann.csv",colClasses=c("character","numeric","character",rep("numeric",30)),skip=1)
# ^Year Householder Moved into Unit

#We then clean and analyze the ACS data, and merge them with the block groups:
s.ages$Id2 <- substr(s.ages$Id,10,length(s.ages$Id))
# substr -> extract/replace susbstrings in character vector: substr(vector, first element, last element)
# so replaces elements in Id2 with Id?
s.ages$PctB1949 <- (s.ages$Estimate..Total....Built.1940.to.1949+s.ages$Estimate..Total....Built.1939.or.earlier)/s.ages$Estimate..Total.
# ^ % of houses built before 1949
s.ages <- subset(s.ages,select=c(Id2,PctB1949))
# subset -> returns that subset selected. so only need the Id2 and % of house built before 1949 

pov$Id2 <- substr(pov$Id,10,length(pov$Id))
pov$U2pov <- (pov$Estimate..Total.-pov$Estimate..Total....2.00.and.over)/pov$Estimate..Total.
# ^ % of pop with income to poverty level bellow 2.00...make less than twice poverty level
pov <- subset(pov,select=c(Id2,U2pov))

house.time$Id2 <- substr(house.time$Id,10,length(house.time$Id))
house.time$MoveB00 <- 1-((house.time$Estimate..Owner.occupied....Moved.in.2010.or.later+
	house.time$Estimate..Owner.occupied....Moved.in.2000.to.2009+
	house.time$Estimate..Renter.occupied....Moved.in.2010.or.later+
	house.time$Estimate..Renter.occupied....Moved.in.2000.to.2009)/
	house.time$Estimate..Total.)
# ^ % of people who moved in before 2000
house.time <- subset(house.time,select=c(Id2,MoveB00))

# merge -> merge two data frames by common colums or row names
BG <- merge(x=BG,y=s.ages,by.x="GEOID10",by.y="Id2")
BG <- merge(x=BG,y=pov,by.x="GEOID10",by.y="Id2")
BG <- merge(x=BG,y=house.time,by.x="GEOID10",by.y="Id2")
# so now all the census data has been added to the shapefile

#Before performing analysis, we clean the block group data to remove large, mostly empty, block groups. It is also helpful to look
# at the variables on a block group level to see if they look accurate. ACS data is not perfect. Block group level data only exists
# for 5-year estimates, and some characteristics will change substantially over the course of 5 years. Mapping each variable allows
# us to verify the data and identify potential issues. The selected variables seem to work well, but this was a particular concern
# in New Orleans, whose population characteristics have changed rapidly post-Katrina.
BG$BG.index <- 1:nrow(BG)
BG <- subset(BG,select=c(GEOID10,ALAND10,TOTAL_HU,PctB1949,U2pov,MoveB00))
BG <- BG[-26,] #removes Lake Pontchartrain
BG[order(BG$ALAND10, decreasing=TRUE)[1:6],] <- NA
gyr.palette <- colorRampPalette(c("green", "yellow", "red"), space = "rgb")
spplot(BG, zcol="PctB1949", col.regions=gyr.palette(100), main="Percent of Structures Built Before 1949")
spplot(BG, zcol="U2pov", col.regions=gyr.palette(100), main="Percent of Households with Income Less than Twice the Poverty Level")
spplot(BG, zcol="MoveB00", col.regions=gyr.palette(100), main="Percent of Residents in their House Before 2000")

#Now we use our model to estimate the risk of missing a smoke alarm:
vars <- as.data.frame(subset(BG,select=c(PctB1949,U2pov,MoveB00)))
# 'as.data.frame' -> Functions to check if an object is a data frame, or coerce it if possible.
# for coef, im assuming that is from the AlarmRisk.r program but how is that read in?
weights <- coef[c(2:ncol(coef)),1]
# 'coef' -> extracts model coefficients from objects returned by modeling functions.
# ^i don't see why we needed to use that function but whatever
weighted.vars <- sweep(as.matrix(vars),MARGIN=2,weights,`*`)
# 'as.matrix' -> turns its argument into matrix
# 'sweep' -> returns an array obtained from an input array by sweeping out a summary statistic,
# sweep(array,MARGIN,STATS,FUN)
# MARGIN->a vector of indices giving the extents of array which correspond to STATS
# STATS<-weights->the summary statstic which is to be swept out
# FUN->the function to be used to carry out the sweep->'*'->multiplication???
alarm.risk <- coef[1,1]+rowSums(weighted.vars)
# 'rowSums' adds elements of each row together and returns the sum of each row in array
BG$alarm.risk <- 1/(1+exp(-alarm.risk))
# adds this alarm.risk to the shapefile
# but is alarm.risk just one element? how can that be ploted?
spplot(BG, zcol="alarm.risk", col.regions=gyr.palette(100), main="Predicted Risk of Missing a Smoke Alarm")

###Fire Fatalities
#In addition to finding the risk of missing an alarm, we are also interested in which parts of the city are most likely to need an alarm, which are the areas with the highest risk of fire fatalities.
# The main factor behind this is the number of fires. Further, input from NOFD and research by the
# [National Fire Protection Association](http://www.nfpa.org/research/reports-and-statistics/demographics-and-victim-patterns/demographic-and-other-characteristics-related-to-fire-deaths) (NFPA)
# has also shown that small children and the elderly are particularly likely to suffer fire fatalities, so we also account for areas in the cities with high concentrations of those groups.
# First, we read in fire and age data. Age data comes from the 2010 Census (Table P12). Fire data comes from NOFD's records. The data came in the form of addresses that were manually cleaned in Excel and then geocoded using ArcGIS.
setwd("O:\\Projects\\Nolalytics\\Active Projects\\Smoke alarm prioritization\\Data")
# 'setwd' sets the working directory to that in parentheses
# 'Fires5yr'->"This is a shapefile with fires over the past 5 years, which was the only piece that we had to make a request (from NOFD) to get. It required a bit of work outside of R to use. I don’t know how
#  LA’s system of tracking fires works, but NOFD’s system has a field for address but no other geographic identifiers, so I did some manual data cleaning in Excel and used an ArcGIS address locator that
#  the City’s GIS team manages to make it into a shapefile."
fires <- readShapePoly(fn="Fires5yr",proj4string=NOLA.proj)
# 'readShapePoly'-> read in shapefiles as R Spatial objects. "I sometimes find the spatial tools in R to be a bit wonky for reading in data, and sometimes one function or another throws an error. I generally prefer readOGR() but if there are issues I use readShape."
fires <- fires[BG,] #so does this save only the shapefile? what is in BG row?
age <- read.csv("DEC_10_SF1_P12_with_ann.csv",colClasses=c("character","numeric","character",rep("numeric",30)),skip=1)
# is this the right csv? http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=DEC_10_SF1_P12&prodType=table
age$Id2 <- substr(age$Id,10,length(age$Id)) #why 10????
U5.cols <- age[,grepl("Under.5", colnames(age))] #searches for column w/ age under 5
# 'grepl' -> search for matches to argument pattern within each element of a character vector
# grepl(pattern, x) -> searches for pattern in x (the character vector)
age$U5.pct <- rowSums(U5.cols)/age$Total. #????should there be a period there?"
# ^ % of pop under age of 5
A65.cols <- age[,grepl("65.and.66", colnames(age)) |
	grepl("67.to.69", colnames(age)) |
	grepl("70.to.74", colnames(age)) |
	grepl("75.to.79", colnames(age)) |
	grepl("80.to.84", colnames(age)) |
	grepl("85.years", colnames(age))]
age$A65.pct <- rowSums(A65.cols)/age$Total.
# ^ %over age of 65
age <- subset(age,select=c(Id2,Total.,U5.pct,A65.pct))
too.small <- which(age$Total.<20) #set block groups with very small numbers of people to NA
age[too.small,(2:ncol(age))] <- NA
BG <- merge(x=BG, y=age, by.x="GEOID10", by.y="Id2")

#We the find the percent of each risk factor in the block groups, normalized so that the scales are comparable.
# To find the number of fires, we use the `over` function in the `sp` package to find the number of fires in each
# block group (comparable to a spatial join in ArcGIS), and normalize by the number of housing units.
fire.count <- sapply(over(BG, geometry(fires), returnList=TRUE),length)
# 'over' -> consistent spatial overlay for points, grids and polygons: at the spatial locations of object x retrieves the indexes or attributes from spatial object y.
# over(x,y,returnList) -> x->geometry (locations) of the queries. y->layer from which the geometries or attributes are queried
# returnList=TRUE-> a list of length length(x), with list element i the vector of all indices of the geometries in
# y that correspond to the $i$-th geometry in x
# 'geometry'->geometry retrieves the SpatialXxx object from a SpatialXxxDataFrame object, with Xxx Lines, Points, Polygons, Grid, or Pixels.
# sapply->return vector or matrix list after applying a function...this is weird and confusing to me
BG$fires <- fire.count
BG$TOTAL_HU[which(BG$TOTAL_HU<20)] <- NA
BG$fires.per.HU <- BG$fires/BG$TOTAL_HU

BG$fires.norm <- BG$fires.per.HU/max(BG$fires.per.HU,na.rm=TRUE)
BG$U5.norm <- BG$U5.pct/max(BG$U5.pct,na.rm=TRUE)
BG$A65.norm <- BG$A65.pct/max(BG$A65.pct,na.rm=TRUE)

#This allows us to model the risk of suffering a fire fatility based on the number of fires and the percent of population at age extremes.
# The weighting is less precise than for the smoke alarm estimation, but we make the assumption that the presence of fires is the most important factor,
# so we set that at twice the weight of the next highest value. For ages, we use the relative risk of fatalities for individuals younger than 5
# and older than 65 compared to the general population. Per the NFPA (http://www.nfpa.org/research/reports-and-statistics/demographics-and-victim-patterns/demographic-and-other-characteristics-related-to-fire-deaths),
# those under 5 are 1.4 times more likely than the general population to suffer a fire fatality, while those over 65 are 2.3 times more likely, so we use these as weights.
age.risk <- c(1.4,2.3)
rel.risk <- age.risk/max(age.risk)
weights <- c(2*max(rel.risk),rel.risk)
names(weights) <- c("fire","U5","A65")
weighted.risk <- sweep(as.matrix(cbind(BG$fires.norm,BG$U5.norm,BG$A65.norm)),MARGIN=2,weights,`*`)
BG$fire.risk <- rowSums(weighted.risk)
spplot(BG, zcol="fire.risk", col.regions=gyr.palette(100), main="Estimated Risk of Fire Fatalities")

###Final Risk and Implementation

#Now that we have the two inputs for our model, we normalize them so they are on the same scale and then take the average for each block group to get the overall risk.
BG$alarm.risk.norm <- BG$alarm.risk/max(BG$alarm.risk,na.rm=TRUE)
BG$fire.risk.norm <- BG$fire.risk/max(BG$fire.risk,na.rm=TRUE)
BG$total.risk <- rowMeans(cbind(BG$alarm.risk.norm,BG$fire.risk.norm))
spplot(BG, zcol="total.risk", col.regions=gyr.palette(100), main="Combined Risk of Missing Smoke Alarms and Fire Fatalities")

#Because Census block group are administrative divisions that don't always match logically with city streets and neighborhoods, we selected clusters of fire zones that laid in hotspots of high-risk block groups. Fire zones were used because NOFD is familiar with the boundaries and the department has a system tying responsibility for each zone to specific firefighters. Zones were selected interactively with ArcGIS so that approximately 10% of the city was covered, and then spatial joins were used to provide NOFD with all of the addresses within selected zones. NOFD will canvas the addresses to distribute smoke alarms. For additional data collection purposes, NOFD designed a questionnaire that captures the following information:

#* Is this a vacant lot?
#* Is this blighted or dangerous?
#* Is it vacant?
#* Is this a commercial structure?
#* # of residential units
#* Is anyone home?
#* Is there a smoke alarm?
#* Does the alarm work Properly?
#* Can we install one?
#* # of Detectors Installed
#* # of Batteries Installed
