#
# AlarmRisk.R
#
#===================
#
# To make a model that predicts which houses are missing smoke alarms,
#  we first read in the AHS data (this is a very large file, and takes a while to load).
# The data can be accessed from the Census Bureau's AHS page at
#  http://www.census.gov/programs-surveys/ahs/data/2011/ahs-national-and-metropolitan-puf-microdata.html
#  (the file newhouse contains structure-specific data)->tnewhouse.csv
# We then select the variables of interest, as well as key geographic variables.
#  We chose variables for our model that are available at discrete geographies and best predict
#  whether or not a house is missing a smoke alarm: the age of the structure, the occupants' poverty level income,
#  and the length of time the occupants have been in their home. Due to differing characteristics of different regions,
#  it is recommended that different variables are tested for relevance if this model is being applied in other cities.
#
#===================
#
#


setwd("/Users/Rodd/Downloads/Programing/GitHub/Smoke Alarm/smoke-alarm-outreach/")
packages = "/var/folders/8t/4cmph4md5g1f9cnlj_f1_9x40000gn/T//Rtmp9aCZdW/downloaded_packages" # or /Users/Rodd/Downloads/Programing/R/
.libPaths(packages) #gets/sets the library trees within which packages are looked for.

require(reshape)
require(plyr)
require(dplyr)
require(ROCR)

# It should be noted that in the '2011 Read Me.txt' file it says that -9 or Blank = Not Reported, -8 = Refused, -7 =
# -7 = Don't know, -6 = Not Applicable which is why all those values are replaced as NA bellow

# im assuming tnewhouse.csv was renamed to AHSnewHouse.csv
AHS <- read.csv("AHSnewHouse.csv",na.strings=c(-9,-8,-7,-6,"-9","-8","-7","-6","'-9'","'-8'","'-7'","'-6'")) # na.strings = a character vector of strings which are to be interpreted as NA values
geo.vars <- subset(AHS,select=c(SMSA,COUNTY,METRO3,SMOKE,BUILT,POOR,HHMOVE))

#Then we clean the data and filter for New Orleans. Because ACS data is reported as the percent of population in bins rather than exact values, we convert the independent variables from continuous to binary comparisons to a threshhold.
 to.num <- function(vec){ #maybe this is for elements that have letters as first and last digits of each number so that it goes through and removes extra and leaves just number. example: 123<-to.num("a123b")
	if(is.numeric(vec)==FALSE){ #is.numeric tests if it is a number. FALSE->not a number
		vec <- as.character(vec) #turns vec into a string/character
		return(as.numeric(substr(vec,2,nchar(vec)-1)))} #nchar returns lenght of vector. substr extracts the elements from 2 to nchar(vec)-1 from vec. only works if vec 3 or more elements
	else{
    return(vec)
  } #basically this turns vec into a number after it cuts the first 2 elements/digits...but i really don't understand why it is used
}
for(j in 3:ncol(geo.vars)){ #ncol gives number of rows or columns present in argument>should be 7-->c(SMSA,COUNTY,METRO3,SMOKE,BUILT,POOR,HHMOVE)
	if(is.numeric(geo.vars[,j])==FALSE){geo.vars[,j] <- to.num(geo.vars[,j])}
}
geo.vars$SMOKE <- to.num(geo.vars$SMOKE) #did this not happen above^
geo.vars$SMOKE <- as.numeric(gsub(pattern=1,replacement=0,x=geo.vars$SMOKE)) #gsub performs replacement of all matches. so it finds a 1 and replaces with a 0
geo.vars$SMOKE <- as.numeric(gsub(pattern=2,replacement=1,x=geo.vars$SMOKE)) #finds a 2 and replaces with a 1
geo.vars$B1950 <- (geo.vars$BUILT<1950)+0 #will return a 0 for FALSE and 1 for TRUE
geo.vars$B200P <- (geo.vars$POOR<200)+0 #0 for FALSE, 1 for TRUE
geo.vars$MOVEB00 <- (geo.vars$HHMOVE<2000)+0

# 'which' function finds which arguements are TRUE and returns only those
nola.met <- geo.vars[which(as.character(geo.vars$SMSA)=="'5560'"),] #The New Orleans Metro is SMSA code 5560
# the above finds the rows with SMSA = 5560 and stores those rows in nola.met...but I think it shoud have "5560" not "'5560'"
nola <- nola.met[which(as.character(nola.met$COUNTY)=="'071'"),]  #Orleans Parish is County Code 071
# same as nola.met but finds rows in nola.met that have 071 as county

#By looking at the cross tabs, we can see the effect that each variable has on the likelihood of needing a smoke alarm
vars <- subset(nola,select=c(SMOKE,B1950,B200P,MOVEB00)) #subset returns just the subset of vectors/elements identified
sum.stats <- ddply(vars, names(vars)[2:ncol(vars)], summarise,mean = mean(SMOKE,na.rm=TRUE),num = length(SMOKE)) #why only for SMOKE?
# 'ddply'->split data frame, apply function, and return resulsts in a data frame
#  so ddply(data frame to be processed, variables to split data fram by, function to apply to each piece)
# 'names' function returns the set of names of an object
# 'summarise' fctn creates a new data frame with summarized info of everything after the first comma: 'mean' and 'num'
# 'mean' returns mean of object ignoring NA values
# 'length' -> gets length of vector
sum.stats[complete.cases(sum.stats),]
# 'complete.cases' returns a logical vector indicating which cases are complete ie have no missing values
# so sum.stats will return a vector with [TRUE TRUE FALSE TRUE...] where TRUE is complete and FALSE for missing values

#Now we can make a logistic regression model and find the weights:
md <- glm(SMOKE~., data=vars, family=binomial("logit"))
# 'glm'->fiting generalized linear models.
# ->glm(formula or a symbolic description of the model to be fitted, data=optional data frame, 'family' details the model->binomial logistic function)
coef <- summary(md)$coefficients
coef #lists coefficients of the binomial line that fits the data

#We test the model to find how efficient it is at identifying homes in need of smoke alarms compared to a random search:
split.data <- function(data, split=0.6) {
  n <- nrow(data)
  samp <- sample(1:n, n*split) # this just produces a data frame of size 'n*.6' with random numbers in the range 1:n
  train <- data[samp, ] # now about n*split rows are randomly selected from the data
  test <- data[-samp,] # this takes all rows that were not selected and stores them in test
  list(train=train, test=test) # this then returns the list at the end of the function
}
vars.split <- split.data(vars) #vars being the orginal subet created from the csv file
vars.train <- vars.split$train #stores the 'train' data that was split
vars.test <- vars.split$test #stores the 'test' data
md.train <- glm(SMOKE~., data=vars.train, family=binomial("logit")) #this creates a fits a line to the 'train' data
predicted <- predict(md.train, vars.test, type="response") #this models the test data with the line created from earlier
# 'predict' creates a prediction from results of various model fitting functions
pred.df <- data.frame(cbind(pred=predicted,actual=vars.test$SMOKE)) #combines predicted and actual results into a dataframe
# 'data.frame' creates data frames
# 'cbind' takes vectors/matrix or data frames and combines by columns or rows
pred.df <- pred.df[complete.cases(pred.df),]
# 'complete.cases' returns a logical vector indicating which cases are complete
pred.df <- pred.df[order(-pred.df$pred),]
# 'order' orders the data fram in descending (because of negative sign) order by 'pred' agrument...i think
pred <- prediction(pred.df$pred,pred.df$actual)
# what is 'prediction' ???
perf <- performance(pred,"tpr","rpp")
# what is 'performance' ???
plot(perf,,xlab="Percent of All Houses Visited",ylab="Percent of Houses Needing Alarms Found", main="Estimated Smoke Alarm Distribution by Total Coverage")
abline(0,1,col="grey",lty=2)

#Now we can save our regression weights and use our model to target specific areas.
save(coef,file="weights.RData")
