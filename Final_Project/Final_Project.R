
# Karina Sirabian
# 11/11/20
# Final Project ENVST206
# This code takes in multiple csv files (one per year) from the 
# EPA's air quality website: 
# https://www.epa.gov/outdoor-air-quality-data/download-daily-data
# The following code compares the air quality of two different period of the
# year for all years given in the data sets and analyzes the air quality 
# differences between those two periods

################################################################################
############################ Imports/Libraries #################################
################################################################################

install.packages(c("TDPanalysis", "ggpubr"))
library(TDPanalysis)
library(ggplot2)
library(gtable)
library(ggpubr)

################################################################################
################################# FUNCTIONS ####################################
################################################################################

# Converts the data column to data format for a dataframe and adds
# a column for the day and month of each date
fix_date <- function(df){
  # Convert data columns to date format
  df$Date <- as.Date(df$Date, "%m/%d/%Y")
  # Add a month column to data
  df$month <- format(df$Date, format = "%m")
  # Add a day column to data
  df$day <- format(df$Date, format = "%d")
  # Add a year column to data
  df$year <- format(df$Date, format = "%Y")
  return(df)
  
}

# Function that returns true if a data falls withing a given period
# The period parameter is a list in the format (start day, start month, end day,
# end month). If my_day and my_month are not in period then false is returned
date_in_period <- function(my_day, my_month, period){
  # Defines start and end dates and months of period from parameters
  start_day <- period[1]
  start_month <- period[2]
  end_day <- period[3]
  end_month <- period[4]
  # Returns True if date is in a given period and False if it isn't
  if (my_month == start_month && my_day >= start_day){
    result <- TRUE
  }
  else if (my_month > start_month && my_month < end_month){
    result <- TRUE
  }
  else if (my_month == end_month && my_day <= end_day){
    result <- TRUE
  }
  else{
    result <- FALSE
  }
  return(result)
  
}


# Checks if a dataframe has more than 1 POC value. If it does not returns True
# If it does, it returns False and it returns a list of data frames where each
# data frame has information from each POC
multiple_POC <- function(df){
  # Gets list of all POCs
  POCs <- levels(as.factor(df$POC))
  # Return True if only one POC
  if (length(POCs) == 1){
    return(c(TRUE))
  }
  # Return False if more than one POC and returns separate dataframes separating
  # the orignal dataframe by POC
  new_frames <- split(df, df$POC)
  str(new_frames)
  return(c(FALSE,new_frames))
}


# Runs tests for a given dataframe to see if the assumptions are met for 
# running the ANOVA test for looking at the PM2.5 measurements for each year
ANOVA_assumptions  <- function(df){
  # Testing Normality for all years
  passed <- TRUE
  for (val in levels(as.factor(df$year))){
    result <- shapiro.test(df$Daily.Mean.PM2.5.Concentration[df$year == val])
    if (result[2] > 0.05){
      print(paste(val, " type is normally distributed"))
    } 
    else {
      print(paste(val, "type is not normally distributed"))
      passed <- FALSE
      }
    print(paste("with a p-value of", toString(result[2])))
  }
  
  b_test <- bartlett.test(df$Daily.Mean.PM2.5.Concentration ~ df$year)
  print(b_test)
  if (b_test[3] > 0.05){
    print("Variance is relatively equal between years")
  }
  else{
    print("Variance is not relatively equal between years")
    passed <- FALSE
  }
  return(passed)
}

# Run the ANOVA test for a given dataframe to look at the PM2.5 measurements
# for each year. Returns true if test was ran (assumptions met) and false if 
# assumptions were not met
run_ANOVA <- function(df){
  # Check assumptions
  if (ANOVA_assumptions(df) == FALSE){
    print('ANOVA test was not ran because assumptions were not met')
    return(FALSE)
  }
  # specify model for species richness and urban type
  in.mod <- lm(df$Daily.Mean.PM2.5.Concentration ~ df$year)
  # run the ANOVA
  in.aov <- aov(in.mod)
  # print out ANOVA table
  print(summary(in.aov))
  # run Tukey HSD
  tukeyT <- TukeyHSD(in.aov)
  # view results
  print(tukeyT)
  # plot entire tukey test
  # make axes labels smaller than usual to fit on plot using cex.axis
  plot(tukeyT, cex.axis=0.75)
  return(TRUE)
}


# Run Kruskal test
# Takes dataframe and runs Kruskal Wallis test for given site
# IF site has multiple POC's, runs Kruskal Wallis Test for multiple POCS
run_kruskal <- function(df, site_name){
  # Creates new dataframe with only given site
  new_df <- df[df$Site.Name == site_name, ]
  # Checks if there are multiple POCs
  POC_check <- multiple_POC(new_df)
  if(POC_check[1] == TRUE){
    # Only one POC
    print(paste("Running Kruskal Wallis Test for",
                site_name))
    print(kruskal.test(new_df$Daily.Mean.PM2.5.Concentration ~ new_df$year))
    print(pairwise.wilcox.test(new_df$Daily.Mean.PM2.5.Concentration,
                               new_df$year,
                               p.adjust.method = "BH"))
  }
  else{
    # More than one POc
    pocs_mindex <- length(POC_check)
    i <- 2
    while (i < pocs_mindex + 1){
      new_df <- POC_check[[i]]
      print(paste('Running Kruskal Wallis Test for ',
                  site_name,'with POC of',
                  toString(new_df$POC[2])))
      print(kruskal.test(new_df$Daily.Mean.PM2.5.Concentration ~ new_df$year))
      print(pairwise.wilcox.test(new_df$Daily.Mean.PM2.5.Concentration,
                                 new_df$year,
                                 p.adjust.method = "BH"))
      i <- i + 1
    }
  }
}


# Prints the min, 1st quartile, median, 3rd quartile, and max for a site and year
# given a dataframe
print_sum_stats <- function(df, site_name, year){
  new_df <- df[df$Site.Name == site_name, ]
  new_df <- new_df[new_df$year == year, ]
  POC_check <- multiple_POC(new_df)
  if(POC_check[1] == TRUE){
    # Only one POC
    print(paste("Printing Summary Statistics for",
                site_name))
    # Prints min, max, median, and 1st and 3rd quartiles 
    print(quantile(new_df$Daily.Mean.PM2.5.Concentration))
    print(paste('Mean:', mean(new_df$Daily.Mean.PM2.5.Concentration)))
    print(paste('SD:', sd(new_df$Daily.Mean.PM2.5.Concentration)))
    
  }
  else{
    # Multiple POCS
    pocs_mindex <- length(POC_check)
    i <- 2
    while (i < pocs_mindex + 1){
      new_df <- POC_check[[i]]
      print(paste('Printing Summary Statistics for ',
                  site_name,'with POC of',
                  toString(new_df$POC[2])))
      print(quantile(new_df$Daily.Mean.PM2.5.Concentration))
      print(paste('Mean:', mean(new_df$Daily.Mean.PM2.5.Concentration)))
      print(paste('SD:', sd(new_df$Daily.Mean.PM2.5.Concentration)))
      i <- i + 1
    }
  }
  
}

# Given a dataframe with a column of labels a-l (for each group on x-axis)
# and a site name, graphs adjacent violin plots for each year for each period
make_violin_plots <- function(df, site){
  # Create Violin Plots
  my_df1 <- df[df$Site.Name == site,]
  # Add color column to dataframe
  my_df1$Legend <- ifelse(my_df1$pqperiod == TRUE,
                         "NYC Pre-Shutdown Period\n(Jan 1st - March 16th)",
                         "NYC Shutdown Period\n(April 1st - June 8th)")
  ggplot(data = my_df1, aes(x=xaxis, y=Daily.Mean.PM2.5.Concentration, color = Legend))+
    geom_violin()+ 
    geom_boxplot(width=0.2,size=0.25)+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.key.size = unit(1, 'cm'))+
    labs(title = paste("Air Pollution in", site),
         y = expression(paste("Daily Mean PM2.5 Concentration (",mu,"g/m"^3*")")),
         x = "Year")+
    scale_x_discrete(labels = c("a" = "2015", "b" = "2015", "c" = "2016",
                                "d" = "2016", "e" = "2017", "f" = "2017",
                                "g" = "2018", "h" = "2018", "i" = "2019",
                                "j" = "2019", "k" = "2020", "l" = "2020"))+
    # Alternate color of violin plot to specify pre-shutdown and shutdown period
    scale_color_manual(values = c('#000099', '#4C9900'))
    
}



################################################################################
################################ MAIN SCRIPT ###################################
################################################################################

# Read in air pollution data from NY city area
pm_2015 <- read.csv("/Users/russell/GitHub/ENVST_206/Final_Project/PM25_2015.csv")
pm_2016 <- read.csv("/Users/russell/GitHub/ENVST_206/Final_Project/PM25_2016.csv")
pm_2017 <- read.csv("/Users/russell/GitHub/ENVST_206/Final_Project/PM25_2017.csv")
pm_2018 <- read.csv("/Users/russell/GitHub/ENVST_206/Final_Project/PM25_2018.csv")
pm_2019 <- read.csv("/Users/russell/GitHub/ENVST_206/Final_Project/PM25_2019.csv")
pm_2020 <- read.csv("/Users/russell/GitHub/ENVST_206/Final_Project/PM25_2020.csv")

# Look at sites for each dataframe
sites_pm15 <- levels(as.factor(pm_2015$Site.Name))
sites_pm16 <- levels(as.factor(pm_2016$Site.Name))
sites_pm17 <- levels(as.factor(pm_2017$Site.Name))
sites_pm18 <- levels(as.factor(pm_2018$Site.Name))
sites_pm19 <- levels(as.factor(pm_2019$Site.Name))
sites_pm20 <- levels(as.factor(pm_2020$Site.Name))

###################### Master Date Frame to use for analyzing ##################

# Get names of sites in all data sets data sets
my_sites <- Reduce(intersect, list(sites_pm15,
                                   sites_pm16,
                                   sites_pm17,
                                   sites_pm18,
                                   sites_pm19,
                                   sites_pm20))

# Combine all PM data into one data frame
pm_all <- rbind(pm_2015, pm_2016, pm_2017, pm_2018, pm_2019, pm_2020)

# Add day of year column
pm_all$doy <- date.to.DOY(pm_all$Date, format = "mm/dd/yyyy")

# Add year, month, day column to PM 
pm_all <- fix_date(pm_all)

# Filter out sites not in all data sets
pm_fsites <- pm_all[pm_all$Site.Name %in% my_sites ,]

# Define quarantine period from April 1st to June 8th
# List in form of start day, start month, end day, end month
q_period <- list(1,4,8,6)

# Define pre-shutdown period to be from January 1st to March 16th
pq_period <- list(1,1,16,3)

# Add column for all dates that are in the quarantine period and add a column 
# for all dates that are in the pre-quarantine period
i <- 1
num_rows <- nrow(pm_fsites)
in_qperiod <- list()
in_pqperiod <- list()
while(i < num_rows+1){
  in_qperiod <- append(in_qperiod,
                       date_in_period(strtoi(pm_fsites$day[i], base = 10L),
                                      strtoi(pm_fsites$month[i], base = 10L),
                                      q_period))
  in_pqperiod <- append(in_pqperiod,
                       date_in_period(strtoi(pm_fsites$day[i], base = 10L),
                                      strtoi(pm_fsites$month[i], base = 10L),
                                      pq_period))
  i <- i + 1
}

# Add column to data frame that says if date was in the shutdown period
pm_fsites$qperiod <- in_qperiod
# Add column to data frame that says if data was in the pre-shutdown period
pm_fsites$pqperiod <- in_pqperiod

# Look at data from months of shutdown
pm_shutdown <- pm_fsites[pm_fsites$qperiod == TRUE,]

# Look at sites during shut down period
pm_shutdown_2020 <- pm_shutdown[pm_shutdown$year == "2020" ,]

# Get all "essential sites" (sites open during shut down)
essential_sites <- levels(as.factor(pm_shutdown_2020$Site.Name))

# Filter out data from sites not open during the shut down
pm_master_data <- pm_fsites[pm_fsites$Site.Name %in% essential_sites ,]

# Filter out data not in NY State
pm_master_data <- pm_master_data[pm_master_data$STATE == "New York",]

#################### Final master data frame is created ########################

### Parse out data for just sites in NYC ###

# Get list of all sites in master data frame
sites <- levels(as.factor(pm_master_data$Site.Name))

# Get list of all counties in master data frame
counties <- levels(as.factor(pm_master_data$COUNTY))

# Make data frame of with sites only in NYC
pm_data_nyc <- pm_master_data[pm_master_data$COUNTY %in% c('Bronx',
                                                           'Kings', 
                                                           'New York',
                                                           'Queens',
                                                           'Richmond'),]
# Get list of all sites in NYC
nyc_sites <- levels(as.factor(pm_data_nyc$Site.Name))

############################# Start Analyzing ##################################

# Look at data from days of shutdown
pm_master_shutdown <- pm_master_data[pm_master_data$qperiod == TRUE,]

# Look at data from days of pre-shutdown
pm_master_preshutdown <- pm_master_data[pm_master_data$pqperiod == TRUE,]

# Runs the ANOVA test for each site for the shutdown period
# Creates an array of results to see if assumptions were met or not. 
results <- list()
for (site in nyc_sites){
  # Filter df to only have data from one site
  my_df1 <- pm_master_shutdown[pm_master_shutdown$Site.Name == toString(site),]
  # Check if site has multiple POCs
  POC_check <- multiple_POC(my_df1)
  if (POC_check[[1]] == FALSE){
    # There are more than one POC for site
    pocs_mindex <- length(POC_check)
    i <- 2
    while (i < pocs_mindex + 1){
      df <- POC_check[[i]]
      ran <- run_ANOVA(df)
      results <- append(results, ran)
      i <- i + 1
    }
    
  }
  else{
    # There is one POC for site
    ran <- run_ANOVA(my_df1)
    results <- append(results, ran)
  }
  
}

# Check if any site met the assumptions of the ANOVA test
print(TRUE %in% results)

# Runs the ANOVA test for each site for the pre shutdown period
# Creates an array of results to see if assumptions were met or not. 

results <- list()
for (site in nyc_sites){
  # Filter df to only have data from one site
  my_df1 <- pm_master_preshutdown[pm_master_preshutdown$Site.Name == toString(site),]
  # Check if site has multiple POCs
  POC_check <- multiple_POC(my_df1)
  if (POC_check[[1]] == FALSE){
    # There are more than one POC for site
    pocs_mindex <- length(POC_check)
    i <- 2
    while (i < pocs_mindex + 1){
      df <- POC_check[[i]]
      ran <- run_ANOVA(df)
      results <- append(results, ran)
      i <- i + 1
    }
    
  }
  else{
    # There is one POC for site
    ran <- run_ANOVA(my_df1)
    results <- append(results, ran)
  }
  
}

# Check if any site met the assumptions of the ANOVA test
print(TRUE %in% results)


########################### ANOVA Test Failed ##################################

######################## Run Kruskal-Wallis Test ###############################

# Look at sites
nyc_sites

# Only look at sites that have one POC
nyc_sites_1poc <- c()
for (site in nyc_sites){
  my_df1 <- pm_master_data[pm_master_data$Site.Name == toString(site),]
  poc_check <- multiple_POC(my_df1)
  if (poc_check[[1]] == TRUE){
    nyc_sites_1poc <- append(nyc_sites_1poc, site)
  }
}

nyc_sites_1poc

# Choose site to run Kruskal Test on (choose from nyc_sites_1poc)
my_site <- "CCNY"
run_kruskal(pm_master_preshutdown, my_site)
run_kruskal(pm_master_shutdown, my_site)

# Calculate summary statistics for given year and site
# choose years from 2015 to 2020
year = "2015"
print_sum_stats(pm_master_preshutdown, my_site, year)
print_sum_stats(pm_master_shutdown, my_site, year)


####################### Make violin plots showing data #########################

### For each site, make a violin plot for each year and for each period ###
### (pre shutdown and shutdown) ###

# Add column to master data frame specifying x-axis name 
# Breaks up data into groups 12 groups (a-l) by each period (2 period) 
# by each year (6 years). Data from dates after shutdown period is given a 0
i <- 1
num_rows <- nrow(pm_master_data)
x_axis <- c()
while(i < num_rows+1){
  if(pm_master_data$qperiod[i] == FALSE && pm_master_data$pqperiod[i] == FALSE){
    # Data does not fall into any category (will not get graphed)
    x_axis <- append(x_axis, "0")
  }
  if(pm_master_data$pqperiod[i] == TRUE){
    # In pre-shutdown period
    if(pm_master_data$year[i] == "2015"){
      x_axis <- append(x_axis, "a")
    }
    if(pm_master_data$year[i] == "2016"){
      x_axis <- append(x_axis, "c")
    }
    if(pm_master_data$year[i] == "2017"){
      x_axis <- append(x_axis, "e")
    }
    if(pm_master_data$year[i] == "2018"){
      x_axis <- append(x_axis, "g")
    }
    if(pm_master_data$year[i] == "2019"){
      x_axis <- append(x_axis, "i")
    }
    if(pm_master_data$year[i] == "2020"){
      x_axis <- append(x_axis, "k")
    }
  }
  if(pm_master_data$qperiod[i] == TRUE){
    # In Shutdown period
    if(pm_master_data$year[i] == "2015"){
      x_axis <- append(x_axis, "b")
    }
    if(pm_master_data$year[i] == "2016"){
      x_axis <- append(x_axis, "d")
    }
    if(pm_master_data$year[i] == "2017"){
      x_axis <- append(x_axis, "f")
    }
    if(pm_master_data$year[i] == "2018"){
      x_axis <- append(x_axis, "h")
    }
    if(pm_master_data$year[i] == "2019"){
      x_axis <- append(x_axis, "j")
    }
    if(pm_master_data$year[i] == "2020"){
      x_axis <- append(x_axis, "l")
    }
  }
  i <- i + 1
}
# Add column of lables to master data frame
pm_master_data$xaxis <- x_axis

# Get rid of data after the shutdown period
graph_data <- pm_master_data[pm_master_data$xaxis != "0",]

# Sort master frame in order of x-axis name
graph_data <- graph_data[order(graph_data$xaxis),]

# Put all violin plots side by side so its easier to look at individual years
# Do this for each site in nyc_sites_1poc
site <- "CCNY"
make_violin_plots(graph_data, site)

####### Look at the difference of the medians of each year for each site #######

### Make a new dataframe with the median of label ###
sites <- c()
year <- c()
medians <- c()
groups <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j","k", "l")
for (site in nyc_sites_1poc){
  # Only look at one site at a time
  df <- graph_data[graph_data$Site.Name == toString(site) ,]
  for (group in groups){
    # Look at one group at a time (a-l)
    df1 <- df[df$xaxis == group ,]
    # Get median of each group
    medians <- append(medians, median(df1$Daily.Mean.PM2.5.Concentration))
    # Get current site
    sites <- append(sites, site)
    # Get year corresponding with label/group
    if (group %in% c("a", "b")){
      cur_year <- "2015"
    }
    if (group %in% c("c", "d")){
      cur_year <- "2016"
    }
    if (group %in% c("e", "f")){
      cur_year <- "2017"
    }
    if (group %in% c("g", "h")){
      cur_year <- "2018"
    }
    if (group %in% c("i", "j")){
      cur_year <- "2019"
    }
    if (group %in% c("k", "l")){
      cur_year <- "2020"
    }
    year <- append(year, cur_year)
  }
}

# Make a new dataframe of medians of each of the 12 groups for each site
median_df <- data.frame("Site" = sites, "year" = year, "Median" = medians)


### Make a data frame of the difference in the medians in each year between
### the pre-shutdown and shutdown period for each site ###
differences <- c()
year <- c()
site <- c()
i <- 1
num_rows <- nrow(median_df)
while(i < num_rows+1){
  # Calculate difference in medians from pre-shutdown to shutdown
  median_diff <- median_df$Median[i] - median_df$Median[i + 1]
  differences <- append(differences, median_diff)
  year <- append(year, median_df$year[i])
  site <- append(site, median_df$Site[i])
  i <- i + 2
}

# Make data frame of difference in medians between periods for each site
# for each year
dif_median_df <- data.frame("Site" = site,
                            "year" = year,
                            "Difference" = differences)

# Plot the difference in the medians of the pre-shutdown and shutdown period
# for each year

dif_median_df$order <- c(1:nrow(dif_median_df))
# Used website to create color palette for discrete variables
#https://colorbrewer2.org/#type=qualitative&scheme=Paired&n=9

# Create list of colors for each site
colors <- c("#A6CEE3", "#E31A1C", "#FDBF6F","#33A02C", "#FB9A99", "#1F78B4", 
            "#B2DF8A", "#FF7F00", "#CAB2D6" )
# Repeat these colors 6 times for 6 years
colors_rep <- rep(colors, each = 6 )
# Make bar plot with the difference of the median PM2.5 Concentrations for each
# year for each site
title <- paste("The Difference of the Median Daily Mean PM2.5 Concentrations\n",
               "Between The Pre-Shutdown and Shutdown Period in New York City")
barplot(dif_median_df$Difference, names.arg = dif_median_df$year,
        col = colors_rep,
        xlab = "Year",
        ylab = bquote(atop("Pre-Shutdown Period minus Shutdown Period Median",
                           "Daily Mean PM2.5 Concentration (" ~mu ~"g/" ~ m ^ 3 ~")")),
        main = title,
        las = 3
)


# Add extra space to right of plot area; change clipping to figure
par(mar=c(6, 8, 6, 8.2), xpd=TRUE)
# Add legend to specify sites
legend("topright", inset = c(-0.2, -0.09), 
       legend = nyc_sites_1poc, bty = "n",
       fill  = colors,
       title= "Legend: NYC Sites")


