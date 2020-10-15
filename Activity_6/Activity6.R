# Activity 6
# 10/9/20

install.packages(c("sp", "rgdal", "dplyr"))

# packages for vector data
library(sp)
# packages for reading in spatial data
library(rgdal)
# data management packages
library(dplyr)

# read in shapefiles
# readOGR in rgdal does this
# glaciers in 1966
g1966 <- readOGR("/Users/russell/GitHub/ENVST_206/Activity_6/a06/GNPglaciers/GNPglaciers_1966.shp")

# glaciers in 2015
g2015 <- readOGR("/Users/russell/GitHub/ENVST_206/Activity_6/a06/GNPglaciers/GNPglaciers_2015.shp")

str(g2015)

# map the glaciers filling in the polygons with light blue and making the borders grey
plot(g1966, col = "lightblue2", border = "grey50")

# data stores all accompanying info/measurements for each spatial object
head(g2015@data)

# Projection info of vector object
g1966@proj4string
g2015@proj4string

# Check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME

# fix glacier name so that is is consistent with the enitre time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(g2015@data$GLACNAME == "Miche Wabun",
                                     "Miche Wabun Glacier",
                                     as.character(g2015@data$GLACNAME)))

# lets combine area, first work with a smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)
gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

# join all data tables by glacier name
gAll <- full_join(gdf66, gdf15, by = "GLACNAME")

# Testing different joins
left <- left_join(gdf66, gdf15, by = "GLACNAME")
right <- right_join(gdf66, gdf15, by = "GLACNAME")
inner <- inner_join(gdf66, gdf15, by = "GLACNAME")
semi <- semi_join(gdf66, gdf15, by = "GLACNAME")
anti <- anti_join(gdf66, gdf15, by = "GLACNAME")
nest <- nest_join(gdf66, gdf15, by = "GLACNAME")

# calculate the % change in area from 1966 to 2015
gAll$gdiff <- ((gAll$area66 - gAll$area15)/gAll$area66)*100

# Make scatter plot of glacier area in 1966 vs percent change in area
plot(gAll$area66, gAll$gdiff,
     pch = 19,
     col = "blue",
     xlab = expression('Glacier Area (km'^2*")"),
     ylab = "Percent Change in Area",
     main = "Glacier Area in 1966 vs Percent Change in Area by 2015")

# join data with the spatial data table and oerwrite into spatial data table
g1966@data <- left_join(g1966@data, gAll, by = "GLACNAME")

# use spplot to shade polygons based on the % change of labels
# first argument is the spatial object
# second is the column of data to daisplay with the different colors
# col changes the color of the borders
spplot(g1966, "gdiff", main = "Percent Change in Glacier Area", col = "transparent")

# look at the Vulture glacier in 1966
vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col = "slategray")

# Calculate mean percent loss
mean_loss <- mean(gAll$gdiff)

# Calculate standard deviation percent loss
std_loss <- sd(gAll$gdiff)

# Maximum deviation percent loss
max_loss <- max(gAll$gdiff)
# Find index of max loss
max_index <- which(gAll$gdiff == max_loss)
# Find name of glacier with max loss
max_name <- gAll$GLACNAME[max_index]

# Minimum deviation percent loss
min_loss <- min(gAll$gdiff)
# Find index of min loss
min_index <- which(gAll$gdiff == min_loss)
# Find name of glacier with min loss
min_name <- gAll$GLACNAME[min_index]


# Glacier with smallest area in 1966
min_area <- min(gAll$area66)
# Find index of min area
min_area_index <- which(gAll$area66 == min_area)
# Find name of glacier with min area
min_area_name <- gAll$GLACNAME[min_area_index]
# Percent change of min area glacier
min_area_change <- gAll$gdiff[min_area_index]


# Glacier with largest area in 1966
max_area <- max(gAll$area66)
# Find index of max area
max_area_index <- which(gAll$area66 == max_area)
# Find name of glacier with max area
max_area_name <- gAll$GLACNAME[max_area_index]
# Percent change of max area glacier
max_area_change <- gAll$gdiff[max_area_index]


# Testing trends in data
shapiro.test(gAll$area66)
plot(gAll$area66)
boxplot(gAll$area66)
boxplot(gAll$gdiff)

x <- lm(gAll$area66 ~ gAll$gdiff)
summary(x)

y <- rstandard(x)

### Check assumptions ###

## Check normality of residuals
# set up qq plot
qqnorm(y)
# add qq line
qqline(y)
# Check normality using shaprio-wilks test
# (Not good for data over 1000 observations)
shapiro.test(y)


par(mfrow=c(1,1))
# Look at glaicer with largest percent loss (boulder)
boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
plot(boulder66, main = "Footprint of Boulder Glacier in 1966", col = "slategray")
boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(boulder15, main = "Boulder Glacier in 2015", col = "slategray")

plot(boulder66, col = "blue", main = "Footprint of Boulder Glacier")
plot(boulder15, col = "red", add = TRUE)
legend("bottomleft",
       c("1966", "2015"),
       fill = c("blue", "red"),
       bty = "n")


# Look at glacier with smallest percent loss (pumpelly)
pump66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
plot(pump66, main = "Pumpelly Glacier in 1966", col = "slategray")

pump15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(pump15, main = "Pumpelly Glacier in 2015", col = "slategray")

plot(pump66, col = "blue", main = "Footprint of Pumpelly Glacier")
plot(pump15, col = "red", add = TRUE)
legend("topleft",
       c("1966", "2015"),
       fill = c("blue", "red"),
       bty = "n")








