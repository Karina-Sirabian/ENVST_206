#activity 2
#9/4/20

###############################################################################
################################### Vectors ###################################
###############################################################################
# make a vector of tree heights in meters
heights <- c(30,41,20,22)
# convert to cm
heights_cm <- heights*100
heights_cm

###############################################################################
################################### Matrices ##################################
###############################################################################
# get more info on matrix function
help(matrix)
# set up matrix with 2 columns and fill in by rows
# first argument is the vector of numbers to fill in the matrix
Mat <- matrix(c(1,2,3,4,5,6), ncol = 2, byrow = TRUE)
Mat
# set up matrix that fills in by columns
# first argument is the vector of numbers to fill in the matrix
Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol = 2, byrow=FALSE)
Mat.bycol
# subset the matrix to look at row 1, column2
Mat.bycol[1,2]
# look at all values in row 1
Mat.bycol[1,]
# look at all values in column 2
Mat.bycol[,2]

###############################################################################
############## Read in weather station file and create data frame #############
###############################################################################
datW <- read.csv("/Users/russell/Github/ENVST_206/Activity2_Data/noaa2011124.csv")
# Get more information about data frame
str(datW)

###############################################################################
################################# Question 2 ##################################
###############################################################################
# Create an example vector with the following objects:
# character, numeric, integer, factor

# Create vector with character objects
character_vector <- c('ab', 'cde', 'fghi', 'jklm', 'nopqr')
# Create vector with numeric objects
numeric_vector <- c(-10.5, 3.6, -9, -6.7, 1000.555)
# Create vector with integer objects
integer_vector <- c(1,20, 300, 4000, 5005)
# Create vector with logical objects
logical_vector <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
# Create factors
directions <- c('North', 'East', 'West', 'East', 'South', 'North')
fdirections <- factor(directions, levels = c('North', 'South', 'East', 'West'))

letters <- c('a', 'b', 'c', 'a', 'b', 'b','b', 'c')
fletters <- as.factor(letters)

numbers <- c('one', 'three', 'two', 'three', 'two')
fnumbers  <- factor(numbers, levels = c('one', 'two', 'three'))

seasons <- c('Winter', 'Fall', 'Winter', 'Summer', 'Spring', 'Fall')
fseasons <- as.factor(seasons)

colors <- c('red', 'blue', 'green', 'red', 'blue', 'red', 'green', 'red')
fcolors <- as.factor(colors)

# Create vector with factor objects
factor_vector <- c(fdirections, fletters, fnumbers, fseasons, fcolors)


###############################################################################
################################# Question 3 ##################################
###############################################################################
# Convert NAME column into a factor
datW$NAME <- as.factor(datW$NAME)
# Find out all unique site names
levels(datW$NAME)
# look at mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
# Ignore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
# Standard Deviation of the maximum temperature for Aberdeen
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
# Average daily temperature (temperature between the min and max)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
# Get the mean across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
# Change column names 
colnames(averageTemp) <- c("NAME", "MAAT")
# Convert level to number for factor data type
datW$siteN <- as.numeric(datW$NAME)
# Create histogram for the first site in our levels
hist(datW$TAVE[datW$siteN == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = expression("Average daily temperature " (degree*C)),
     ylab = "Relative frequency",
     col = "#00FFFF",
     border = "#FF0000")
# Look up arguments in hist functions
help(hist)

###############################################################################
################################# Question 4 ##################################
###############################################################################
# Create a histogram for another weather station

# Function that creates a histogram for any weather station given the weather
# station number
plot_station <- function(station_num){
  hist(datW$TAVE[datW$siteN == station_num],
       freq = FALSE,
       main = paste(levels(datW$NAME)[station_num]),
       xlab = expression("Average daily temperature " (degree*C)),
       ylab = "Relative frequency",
       col = "#00FFFF",
       border = "#FF0000")
  return()
  
}

# Creates histogram for Aberdeen
plot_station(1)
# Creates a histogram for Livermore
plot_station(2)
# Creates a histogram for Mandan Experiment Station
plot_station(3)
# Creates a histogram for Mormon Flat
plot_station(4)
# Creates a histogram for Morrisville
plot_station(5)

###############################################################################
################################# Question 5 ##################################
###############################################################################
# Assume climate change increases the mean temperature by 4 degrees Celsius
# in Aberdeen, but the SD stays the same. How often do you expect to observe
# temperatures greater than the current threshold for extreme high temperatures?

# Find the current threshold for extreme high temperatures in Aberdeen
# (Assume that the extreme high threshold is the 95th percentile)
current_high_threshold <- qnorm(0.95,
                                mean(datW$TAVE[datW$siteN == 1], na.rm=TRUE),
                                sd(datW$TAVE[datW$siteN == 1], na.rm =TRUE))
# Find probability that temperature is greater than the current threshold for
# extreme high temperature if climate change increases the mean by 4 degrees C
prob_extreme <- 1 - pnorm(current_high_threshold,
                          mean(datW$TAVE[datW$siteN == 1] + 4, na.rm = TRUE),
                          sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))

###############################################################################
################################# Question 6 ##################################
###############################################################################
# Make a histogram of daily precipitation for Aberdeen

hist(datW$PRCP[datW$siteN == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation (mm)",
     ylab = "Relative frequency",
     col = "#0000FF",
     border = "#000000")

###############################################################################
################################# Question 7 ##################################
###############################################################################
# Find the total annual precipitation for each year and site

#Convert the year column to a factor
datW$year <- as.factor(datW$year)
# Find out all unique years
levels(datW$year)
# Convert level to number for factor data type
datW$yearN <- as.numeric(datW$year)
# Get the sum of precipitation for each year for each site
annual_prcp <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum",
                         na.rm=TRUE)
# Change column names
colnames(annual_prcp) <- c("Site", "Year", "Annual Precipitation")

###############################################################################
################################# Question 8 ##################################
###############################################################################
# Make a histogram of annual precipitation for Aberdeen and Mandan Station ND

# Convert Site column into a factor
annual_prcp$Site <- as.factor(annual_prcp$Site)
# Find out all unique site names
levels(annual_prcp$Site)
# Convert level to number for factor data type
annual_prcp$Site <- as.numeric(annual_prcp$Site)
# Histogram of annual precipitation for Aberdeen
hist(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual Precipitation (mm)",
     ylab = "Relative frequency",
     col = "#514DB7",
     border = "#000000")
# Histogram of annual precipitation for Mandan Experiment Station
hist(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 3],
     freq = FALSE,
     main = paste(levels(datW$NAME)[3]),
     xlab = "Annual Precipitation (mm)",
     ylab = "Relative frequency",
     col = "#3EC161",
     border = "#000000")

# Average annual precipitation in Aberdeen
mean(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 1], na.rm = TRUE)
# Median annual precipitation in Aberdeen
median(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 1], na.rm = TRUE)

# Average annual precipitation in Mandan
mean(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 3], na.rm = TRUE)
# Median annual precipitation in Mandan
median(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 3], na.rm = TRUE)

###############################################################################
################################# Question 9 ##################################
###############################################################################
# How likely is a year with 700 mm of precipitation or less in Mandan 
# vs Aberdeen

# Probability of the annual precipitation being 700 mm or less in Aberdeen
Aberdeen_prob <- pnorm(700,
                       mean(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 1],
                            na.rm = TRUE),
                       sd(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 1],
                          na.rm = TRUE))
# Probability of the annual precipitation being 700 mm or less in Mandan
Mandan_prob <- pnorm(700,
                     mean(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 3],
                          na.rm = TRUE),
                     sd(annual_prcp$`Annual Precipitation`[annual_prcp$Site == 3],
                        na.rm = TRUE))

