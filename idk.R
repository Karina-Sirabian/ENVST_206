# Activity 8
# Project
# 10/20/20

# Read in Census data by country from 1990
# Countries that start with A-M
census_90AM <- read.csv("/Users/russell/GitHub/ENVST_206/Project/Census_90AM.csv", skip = 1)
# Countries that start with N-Z
census_90NZ <- read.csv("/Users/russell/GitHub/ENVST_206/Project/Census_90NZ.csv", skip = 1)

# Combine both data frames from the 1990's
census_90 <- rbind(census_90AM, census_90NZ)

# Read in file of annual mean temperature of countries 
mean_temps <- read.csv("/Users/russell/GitHub/ENVST_206/Project/GlobalLandTemperaturesByCountry.csv")

# Convert year column to date format
mean_temps$dt <- as.Date(mean_temps$dt, "%Y-%m-%d")

# Add year column
mean_temps$year <- format(mean_temps$dt, format = "%Y")

# Filter out data from before 1950
mean_temps <- filter(mean_temps, mean_temps$year > 1949)

##
# Get names of all countries in temperature data set
countries_temp <- as.factor(mean_temps$Country)
levels(countries_temp)

# Get names of all countries in census data set
countries_census <- as.factor(census_90$Country)
levels(countries_census)

# Get names of countries in both data sets
my_countries <- intersect(countries_temp, countries_census)
my_countries

# Filter out countries not in both data sets
census_90f <- filter(census_90, census_90$Country %in% my_countries)
mean_tempsf <- filter(mean_temps, mean_temps$Country %in% my_countries)

# Check countries are equal
census_90f$Country <- as.factor(census_90f$Country)
mean_tempsf$Country <- as.factor(mean_tempsf$Country)
levels(census_90f$Country) == levels(mean_tempsf$Country)

# Get the mean land temperature for each year for each country
annual_temps <- aggregate(mean_tempsf$AverageTemperature,
                          by=list(mean_tempsf$Country, mean_tempsf$year),
                          FUN="mean",
                          na.rm=TRUE)

annual_temps <- na.omit(annual_temps)
# rename columns
colnames(annual_temps) <- c("Country", "year", "MeanAnnualTemp")



# Get all years in data set
years <- as.factor(annual_temps$year)
levels(years)

# Function to create a df with only a single countries data
get_country_temps <- function(country){
  df <- filter(annual_temps, annual_temps$Country == country)
  return(df)
}

greek_temps <- get_country_temps("Greece")
greek_temps
plot(greek_temps$year, greek_temps$MeanAnnualTemp)


#########
# set up regression
temps.mod <- lm(greek_temps$year ~ greek_temps$MeanAnnualTemp)
# get standardized residuals
temps.res <- rstandard(temps.mod)

### Check assumptions ###

## Check normality of residuals
# set up qq plot
qqnorm(temps.res)
# add qq line
qqline(temps.res)
# Check normality using shaprio-wilks test
# (Not good for data over 1000 observations)
shapiro.test(temps.res)

## Check residual plot for equal variance
# make residual plot
plot(greek_temps$year, temps.res,
     xlab = "year",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)

# Interpret results
summary(temps.mod) 


#############################################################################
# Old population files

# Read in file of annual populations of countries 
populations <- read.csv("/Users/russell/GitHub/ENVST_206/Project/Population.csv")

# Edit data frame for useful data #
# Remove data based on gender and data from 2005 (only care about more recent years)
filtered_pops <- filter(populations,
                        populations$SEX == "T",
                        populations$AGE == "T",
                        populations$YEAR != 2005)
# Only want columns of countries, year, and total population 
pops <- data_frame("Country" = filtered_pops$Country,
                   "Year" = filtered_pops$YEAR,
                   "Population" = filtered_pops$Value)

# Look at all countries
countries <- pops$Country
fcountries <- as.factor(countries)
all_countries <- as.vector(levels(fcountries))
all_countries
for (i in c(1:40)){
  print(all_countries[i])
}


# Look at population trends in countries
plot_pop <- function(country){
  df <- filter(pops, pops$Country == country)
  plot(df$Year, df$Population,
       xlab = "Year", ylab = "Population")
  return()
  
}
plot_pop("Latvia")
AUS <- filter(pops, pops$Country == all_countries[1])
plot(AUS$Year, AUS$Population)





