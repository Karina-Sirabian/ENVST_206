# Activity 4
# 9/18/20

# Read in Beaver Dam data
datB <- read.csv("/Users/russell/GitHub/ENVST_206/Activity_4/beaver_dam.csv")

# Create scatter plot
plot(datB$dams.n, datB$area.ha,
     pch = 19,
     col = "royalblue4",
     main = "Number of beaver dams vs surface water area",
     ylab = "Surface water area (ha)",
     xlab = "Number of beaver dams")

# set up regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
# get standardized residuals
dam.res <- rstandard(dam.mod)

### Check assumptions ###

## Check normality of residuals
# set up qq plot
qqnorm(dam.res)
# add qq line
qqline(dam.res)
# Check normality using shaprio-wilks test
# (Not good for data over 1000 observations)
shapiro.test(dam.res)

## Check residual plot for equal variance
# make residual plot
plot(datB$dams.n, dam.res,
     xlab = "beaver dams",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)

# Interpret results
summary(dam.mod) 

# make plot of beaver dams and surface water
plot(datB$dams.n, datB$area.ha,
     pch = 19,
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab = "Number of beaver dams",
     main = "Number of Beaver Dams vs Surface Water Area")

# add regression line
# make line width thicker
abline(dam.mod, lwd=2)

###############################################################################
################################# Question 3 ##################################
###############################################################################

# Read in red maple pheno data
pheno <- read.csv("/Users/russell/GitHub/ENVST_206/Activity_4/red_maple_pheno.csv")

# set up panel of plots with one row and two columns
par(mfrow=c(1,2))

# Plot Day of Leaf Out vs Maximum Temperature
plot(pheno$Tmax, pheno$doy,
     pch = 19,
     col = "royalblue4",
     main = "Day of Leaf Out vs Maximum Temperature",
     ylab = "Day of leaf out",
     xlab = expression("Maximum temperature " (degree*C)))

# Plot Day of Leaf Out vs Precipitation
plot(pheno$Prcp, pheno$doy,
     pch = 19,
     col = "royalblue4",
     main = "Day of Leaf Out vs Precipitation",
     ylab = "Day of leaf out",
     xlab = "Precipitation (mm)")

################################ Latitude ####################################
# Plot Day of Leaf Out vs Latitude
plot(pheno$Lat, pheno$doy,
     pch = 19,
     col = "darkgreen",
     main = "Day of Leaf Out vs Latitude",
     ylab = "Day of leaf out",
     xlab =  "Latitude (degrees)")

# Get Latitude Regression Line
lat.mod <- lm(pheno$doy ~ pheno$Lat)
summary(lat.mod)
abline(lat.mod, lwd=2)
lat.res <- rstandard(lat.mod)

#Check assumptions
qqnorm(lat.res)
qqline(lat.res)
summary(lat.res)

# make residual plot
plot(pheno$Lat, lat.res,
     xlab = "Latitude (degrees)",
     ylab = "standardized residual")

################################ Elevation ####################################
# Plot Day of Leaf Out vs Elevation
plot(pheno$elev, pheno$doy,
     pch = 19,
     col = "royalblue3",
     main = "Day of Leaf Out vs Elevation",
     ylab = "Day of leaf out",
     xlab =  "Elevation (m)")

# Get Elevation Regression Line
elev.mod <- lm(pheno$doy ~ pheno$elev)
summary(elev.mod)
abline(elev.mod, lwd=2)
elev.res <- rstandard(elev.mod)

#Check assumptions
qqnorm(elev.res)
qqline(elev.res)
summary(elev.res)

# make residual plot
plot(pheno$elev, elev.res,
     xlab = "Elevation (m)",
     ylab = "standardized residual")

########################### Maximum Temperature ###############################
# Plot Day of Leaf Out vs Maximum Temperature
plot(pheno$Tmax, pheno$doy,
     pch = 19,
     col = "red",
     main = "Day of Leaf Out vs Maximum Temperature",
     ylab = "Day of leaf out",
     xlab =  expression("Maximum Temperature " (degree*C)))

# Get Maximum Temperature Regression Line
# and check assumptions
max_temp.mod <- lm(pheno$doy ~ pheno$Tmax)
max_temp.res <- rstandard(max_temp.mod)
qqnorm(max_temp.res)
qqline(max_temp.res)

summary(max_temp.mod)

abline(max_temp.mod, lwd=2)

# make residual plot
plot(pheno$elev, elev.res,
     main = "Residual Plot of Elevation and Day of Leaf Out",
     xlab = "Elevation (m)",
     ylab = "standardized residual")
abline(h=0)

############################### Urban/Rural ###################################

# Sort data by Site type
pheno <- pheno[order(pheno$siteDesc),]

# Define sites as a factor 
site_factor <- as.factor(pheno$siteDesc)
# Define colors for points on graph based on site description
site_colors <- c("red", "blue")
site_colors <- site_colors[as.numeric(site_factor)]
# Define shapes of points on graph based on site description
site_shapes = c(16, 17)
site_shapes <- site_shapes[as.numeric(site_factor)]

# Plot box plots to show day of leaf out in urban vs rural sites
plot(site_factor,pheno$doy,
     pch = 19,
     col = c("red", "blue"),
     main = "Day of Leaf Out in Urban and Rural Sites",
     xlab = "Site Description",
     ylab = "Day of leaf out")

# Plot scatter plot of both urban and rural sites
plot(pheno$doy,
     frame = FALSE,
     col = site_colors,
     pch = site_shapes,
     cex = 0.7,
     xaxt = "n",
     main = "Day of Leaf Out in Urban and Rural Sites",
     xlab = "",
     ylab = "Day of leaf out")

# Add legend to plot to specify urban vs rural points
legend(-39,56, legend = levels(site_factor),
       col = c("red", "blue"), pch = c(16, 17))

# Make two plots show side by side
par(mfrow = c(1,2))

# Plot just Rural data points
plot(pheno$doy[pheno$siteDesc == "Rural"],
     pch = 19,
     col = "red",
     main = "Day of Leaf Out in Rural Sites",
     xaxt = "n",
     xlab = "",
     ylab = "Day of leaf out")

# Plot just Urban points
plot(pheno$doy[pheno$siteDesc == "Urban"],
     pch = 19,
     col = "blue",
     main = "Day of Leaf Out in Urban Sites",
     xaxt = "n",
     xlab = "",
     ylab = "Day of leaf out")

# Does the day of Leaf Out differ for Urban and Rural Sites? 

# Use the shapiro-wilks test to look for normality in site
# shapiro-wilk test on grazing plots
shapiro.test(pheno$doy[pheno$siteDesc == "Urban"])
# shapiro-wilk test on grazing exlusion plots
shapiro.test(pheno$doy[pheno$siteDesc == "Rural"])
# Use bartlett test for testing for equal variance
bartlett.test(pheno$doy ~ pheno$siteDesc)
# T-test (2 sample, two-sided t-test)
t.test(pheno$doy ~ pheno$siteDesc)

###############################################################################
################################# Question 4 ##################################
###############################################################################

# Check for multi-collinearity
# Generate series of covariance plots
plot( ~ pheno$Lat + pheno$Tmax + pheno$Tmin + pheno$Prcp + pheno$elev
      + pheno$siteDesc)


###############################################################################
################################# Question 6 ##################################
###############################################################################
# Run Regression
# Build a multiple regression that investigates max temp, precipitation,
# elevation, and urban/rural designation
pheno$urID <- ifelse(pheno$siteDesc == "Urban", 1, 0)

# Set up multiple regression
mlr <- lm(pheno$doy ~ pheno$Tmax + pheno$Prcp + pheno$elev + pheno$urID)
summary(mlr)
# Calculate fitted values from the regression line for each observation
mlFitted <- fitted(mlr)
mlr.res <- residuals(mlr)

# Check for normality in residuals
qqnorm(mlr.res)
qqline(mlr.res)

# Create pheno data frame with rows that have null quantities in the Tmax,
# precipitation, elevation, and urID columns removed
pheno_nona <- na.omit(pheno, cols = c("Tmax", "Prcp","elev","urID"))

# Check for equal variance in residuals
plot(x = mlFitted, y = mlr.res,
     main = "Residual Plot",
     xlab = "Predicted Day of Year",
     ylab = "Residual")
abline(h=0)
