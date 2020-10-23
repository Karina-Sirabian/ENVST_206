
# 10/20/20
# Activity 7

# Read in air pollution data from NY city area
air_2019 <- read.csv("/Users/russell/GitHub/ENVST_206/Activity_7/PM25_2019.csv")
air_2020 <- read.csv("/Users/russell/GitHub/ENVST_206/Activity_7/PM25_2020.csv")

# Convert data columns to date format
air_2019$Date <- as.Date(air_2019$Date, "%m/%d/%Y")
air_2020$Date <- as.Date(air_2020$Date, "%m/%d/%Y")

# Add a month column to data
air_2019$month <- format(air_2019$Date, format = "%m")
air_2020$month <- format(air_2020$Date, format = "%m")

# Add a day column to data
air_2019$day <- format(air_2019$Date, format = "%d")
air_2020$day <- format(air_2020$Date, format = "%d")

# Look at all sites
sites_2019 <- as.factor(air_2019$Site.Name)
levels(sites_2019)
sites_2020 <- as.factor(air_2020$Site.Name)
levels(sites_2020)

# Get names of sites in both data sets
my_sites <- intersect(sites_2019, sites_2020)

# Filter out sites not in both data sets
air_2019f <- filter(air_2019, air_2019$Site.Name %in% my_sites)
air_2020f <- filter(air_2020, air_2020$Site.Name %in% my_sites)

# Check sites are equal
air_2019f$Site.Name <- as.factor(air_2019f$Site.Name)
air_2020f$Site.Name <- as.factor(air_2020f$Site.Name)
levels(air_2019f$Site.Name) == levels(air_2020f$Site.Name)

# Look at air pollution in White Plains in April in 2019 and 2020
mo_2019 <- filter(air_2019f,
                  air_2019f$Site.Name == "MORRISANIA",
                  air_2019f$month == "04")
mo_2020 <- filter(air_2020f,
                  air_2020f$Site.Name == "MORRISANIA",
                  air_2020f$month == "04")

mean(mo_2019$Daily.Mean.PM2.5.Concentration)
mean(mo_2020$Daily.Mean.PM2.5.Concentration)
sd(mo_2019$Daily.Mean.PM2.5.Concentration)
sd(mo_2020$Daily.Mean.PM2.5.Concentration)

plot(mo_2019$day,
     mo_2019$Daily.Mean.PM2.5.Concentration,
     col = "red",
     pch = 19,
     type = "b",
     ylab = "Daily Mean PM2.5 Concentration (micrograms/m^3)",
     xlab = "Day of Month",
     main = "Daily Mean of PM2.5 Concentration in April in Morrisania")

# add 2020
points(mo_2020$day,
       mo_2020$Daily.Mean.PM2.5.Concentration,
       type = "b",
       pch = 19,
       col = "blue")

# add legend
legend("topright",
       c("2019", "2020"),
       col = c("red", "blue"),
       pch = 19,
       lwd = 1,
       bty = "n")





