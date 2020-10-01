# Activity 5 Intro to data visualizations
# 9/25/20

library(ggplot2)
library(colorspace)
hcl_palettes(plot = TRUE)

# read in weather station file from data folder
datW <- read.csv("/Users/russell/GitHub/ENVST_206/Activity_5/noaa2011124.csv")
# specify that the name column should be a factor
datW$NAME <- as.factor(datW$NAME)
# set up a vector of all names for each level
nameS <- levels(datW$NAME)

# make a dataframe with just precipitation, year, and site name
# remove NA using na.omit
datP <- na.omit(data.frame(NAME = datW$NAME,
                          year = datW$year,
                          PRCP = datW$PRCP))
# total annual precipitation (mm)
precip <- aggregate(datW$PRCP,
                    by=list(datW$NAME, datW$year),
                    FUN = "sum", na.rm = TRUE)
# rename columns
colnames(precip) <- c("NAME", "year", "totalP")
# add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN = "length")$x

# make a new dataframe with years not missing too many measurements
pr <- precip[precip$ncount >= 364,]
# look at only livermore california and morrisville new york precipitation
ca <- pr[pr$NAME == nameS[2],]
ny <- pr[pr$NAME == nameS[5],]

# make a plot of california precipitation
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     col = "blue",
     ylab = "Annual Precipitation (mm)",
     yaxt = "n",
     ylim = c(0, 1600),
     xlab = "Year",
     main = "Annual Precipitation in California and New York")
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,1600, by=100), las = 2)

# add new york
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col = "tomato3")

# add legend
legend("topleft",
       c("Livermore, CA", "Morrisville, NY"),
       col = c("blue", "tomato3"),
       pch = 19,
       lwd = 1,
       bty = "n")

# base R plot
plot(pr$year, pr$totalP)

#Only Run Once
#install.packages("ggplot2")

# ggplot
ggplot(data = pr,
       aes(x=year,
           y = totalP,
           color = NAME)) + 
  geom_point()  + 
  geom_path() + 
  labs(x= "Year", y = "Annual Precipitation") +
  theme_classic() + 
  scale_color_manual(values = c("red", "orange", "yellow", "green", "blue"))

###############################################################################
################################# Question 3 ##################################
###############################################################################
# Make a plot comparing mean annual temperature for each year in both 
# Morrisville, New York, and Mandan, ND

# Average daily temperature (temperature between the min and max)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

# make a dataframe with just average temperature, year, and site name
# remove NA using na.omit
datT <- na.omit(data.frame(NAME = datW$NAME,
                           year = datW$year,
                           TAVE = datW$TAVE))
# mean annual temperature
mean_temp <- aggregate(datW$TAVE,
                    by=list(datW$NAME, datW$year),
                    FUN = "mean", na.rm = TRUE)
mean_temp <- na.omit(mean_temp)
# rename columns
colnames(mean_temp) <- c("NAME", "year", "MeanTemp")
# add the x column from aggregate looking at the length of observations in each year
mean_temp$ncount <- aggregate(datT$TAVE, by=list(datT$NAME, datT$year), FUN = "length")$x

# make a new dataframe with years not missing too many measurements
mt <- mean_temp[mean_temp$ncount >= 300,]
# look at only mandan experiment station nd and morrisville ny temperatures
nd <- mt[mt$NAME == nameS[3],]
ny <- mt[mt$NAME == nameS[5],]
dev.off()
# make a plot of north dakota mean annual temperature
plot(nd$year, nd$MeanTemp,
     type = "b",
     pch = 19,
     col = "navy",
     ylab = expression("Mean Annual Temperature " (degree*C)),
     yaxt = "n",
     ylim = c(2, 10),
     xlab = "Year",
     xaxt = "n",
     main = "Mean Annual Temperature in Morrisville, New York and Mandan Experiment Station, North Dakota")
# add y axis
axis(2, seq(2,10, by=1), las = 2)
# add x axis
axis(1, seq(1930,2020, by=5), las = 1)

# add new york
points(ny$year, ny$MeanTemp,
       type = "b",
       pch = 19,
       col = "tomato3")

# add legend
legend("topleft",
       c("Morrisville, NY", "Mandan Experiment Station, ND"),
       col = c("tomato3", "navy"),
       pch = 19,
       lwd = 1,
       cex = 1.25,
       bty = "n")

###############################################################################
################################# Question 4 ##################################
###############################################################################
# Make a plot comparing mean annual temperature for each year in both 
# Morrisville, New York, and Mandan, ND

# Make dataframe with only name, year, and max tempeature
datMaxT <- na.omit(data.frame(NAME = datW$NAME,
                           year = datW$year,
                           TMAX = datW$TMAX))

# Find the mean max temperature for each year and each site
mean_max_temp <- aggregate(datMaxT$TMAX,
                       by=list(datMaxT$NAME, datMaxT$year),
                       FUN = "mean", na.rm = TRUE)

colnames(mean_max_temp) <- c("NAME", "year", "MeanTMAX")
# add the x column from aggregate looking at the length of observations in each year
mean_max_temp$ncount <- aggregate(datMaxT$TMAX, by=list(datMaxT$NAME, datMaxT$year), FUN = "length")$x

# make a new dataframe with years not missing too many measurements
mtmax <- mean_max_temp[mean_max_temp$ncount >= 300,]
# look at only mandan experiment station nd and morrisville ny temperatures
nd_maxt <- mtmax[mtmax$NAME == nameS[3],]
ny_maxt <- mtmax[mtmax$NAME == nameS[5],]


# make a plot of north dakota mean annual max temperature
plot(nd_maxt$year, nd_maxt$MeanTMAX,
     type = "b",
     pch = 19,
     col = "navy",
     ylab = expression("Mean Annual Maximum Temperature " (degree*C)),
     yaxt = "n",
     ylim = c(8, 16),
     xlab = "Year",
     xaxt = "n",
     main = "Mean Annual Maximum Temperature in Morrisville, New York and Mandan Experiment Station, North Dakota")


# add y axis
axis(2, seq(8,16, by=1), las = 2)
# add x axis
axis(1, seq(1930,2020, by=5), las = 1)

# add new york
points(ny_maxt$year, ny_maxt$MeanTMAX,
       type = "b",
       pch = 19,
       col = "tomato3")

# add legend
legend("topleft",
       c("Morrisville, NY", "Mandan Experiment Station, ND"),
       col = c("tomato3", "navy"),
       pch = 19,
       lwd = 1,
       cex = 1.25,
       bty = "n")


###############################################################################
################################# Question 5 ##################################
###############################################################################

# Create new pallete
hcl_palettes(plot = TRUE)
pal <- choose_palette()

# Rearrange order of colors
pal_order = c(pal(5)[3], pal(5)[2], pal(5)[1], pal(5)[4], pal(5)[5])

# Plot annual precipitation of each site
ggplot(data = pr,aes(x = year, y=totalP, color=NAME, shape=NAME))+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+ #make axis labels+
  theme_classic()+
  scale_color_manual(values = pal_order)+
  scale_shape_manual(values = c(8,16,17,15,18))


###############################################################################
################################# Question 6 ##################################
###############################################################################

# Create Violin Plots
ggplot(data = datW, aes(x=NAME, y=TMIN)) + #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25,fill="grey90")+ #add grey boxplots and make  
  # them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic()

# Look at Mormon Flat, AZ
sub <- datW[datW$NAME == nameS[4] & datW$year == 1974,]

# Specify data format
sub$DATE <- as.Date(sub$DATE, "%Y-%m-%d")

###############################################################################
################################# Question 7 ##################################
###############################################################################


# Plot Data vs Max temperature in Mormon Flat
ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximum Temperature (C)")

# Make a bar plot
ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

###############################################################################
################################# Question 8 ##################################
###############################################################################

# Look at ABERDEEN, WA
sub1 <- datW[datW$NAME == nameS[1] & datW$year == 1974,]

# Specify data format
sub1$DATE <- as.Date(sub1$DATE, "%Y-%m-%d")

# Plot Data vs Max temperature in Aberdeen
ggplot(data=sub1, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximum Temperature (C)")+
  ggtitle("Maximum Daily Temperature in Aberdeen, Washington")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Make a bar plot
ggplot(data=sub1, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")+
  ggtitle("Daily Precipitation in Aberdeen, Washington")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


###############################################################################
################################# Question 9 ##################################
###############################################################################

# Look at Aberdeen, WA
sub2 <- datW[datW$NAME == nameS[1] & datW$year > 1999,]

# Check that each year has at least 364 data points
freq_table <- table(sub2$year)  # table of all years and their frequencies
index <- 1

# For loop to remove any years that do not have more than 363 recorded days
for (elem in freq_table){
  if(elem < 364) {sub2 <- sub2[sub2$year!= (names(freq_table)[index]),]}
  index <- index + 1
}

# make the year column a factor
sub2$year <- as.factor(sub2$year)

# Make violin plots for each year in the past decade
ggplot(data = sub2, aes(x=year, y=TMIN))+ 
  geom_violin(fill=rgb(0.933,0.953,0.98))+ 
  geom_boxplot(width=0.2,size=0.25, fill="royalblue3")+ 
  theme_classic() +
  ggtitle("Daily Minimum Temperatures in Aberdeen, Washington")+
  labs(x = "year", y = expression("Daily Minimum Temperature "(degree*C)))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_y_continuous(breaks = c(-10, -5, 0, 5, 10, 15, 20))




