# Activity 3
# 9/11/20

# Read in data from lemming herbivory
ch4 <- read.csv("/Users/russell/GitHub/ENVST_206/Activity_3/lemming_herbivory.csv")
# Convert Herbivory column to a factor
ch4$herbivory <- as.factor(ch4$herbivory)
# Create a box plot of the data
# Methane flux (dependent) ~ (depends on) herbivory (independent)
plot(ch4$CH4_Flux ~ ch4$herbivory, xlab = "Treatment",
     ylab = "CH$ fluxes (mgC m -2 day-1)",
     col = "pink",
     border = "green")

###############################################################################
################################ Question 1 ###################################
###############################################################################
# Do methane fluxes differ when lemmings graze the vegetation? 

# Use the shapiro-wilks test to look for normality in each treatment
# shapiro-wilk test on grazing plots
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])
# shapiro-wilk test on grazing exlusion plots
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
# Use bartlett test for testing for equal variance
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)
# T-test (2 sample, two-sided t-test)
t.test(ch4$CH4_Flux ~ ch4$herbivory)

###############################################################################
################################ Question 4 ###################################
###############################################################################
# Read in data from insect richness
datI <- read.csv("/Users/russell/GitHub/ENVST_206/Activity_3/insect_richness.csv")
# Convert urban names to a factor
datI$urbanName <- as.factor(datI$urbanName)

### Test assumptions of ANOVA Test ###
# Test if each group is normally distributed using shapiro-wilk test

# Testing Normality for all urban types
for (val in levels(datI$urbanName)){
  result <- shapiro.test(datI$Richness[datI$urbanName == val])
  if (result[2] > 0.05) print(paste(val, " type is normally distributed"))
  else print(paste(val, "type is not normally distributed"))
  print(paste("with a p-value of", toString(result[2])))
}

# Test if variance is relatively equal between all urban data types
bartlett.test(datI$Richness ~ datI$urbanName)

###############################################################################
################################ Question 6 ###################################
###############################################################################

# specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
# run the ANOVA
in.aov <- aov(in.mod)
# print out ANOVA table
summary(in.aov)
# run Tukey HSD
tukeyT <- TukeyHSD(in.aov)
# view results
tukeyT
# plot entire tukey test
# make axes labels smaller than usual to fit on plot using cex.axis
plot(tukeyT, cex.axis=0.75)

# Plot Insect Richness for each urban group
plot(datI$Richness ~ datI$urbanName,
     main = "Insect Richness in Different Urban Landcover Groups",
     xlab = "Urban Landcover Group",
     ylab = "Insect Species Richenss",
     col = "blue")

# Calculate means across factor data simultaneously
tapply(datI$Richness, datI$urbanName, "mean")

###############################################################################
################################ Question 8 ###################################
###############################################################################

### Chi-Squared Goodness of Fit Test ###

# set up contingency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE)
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

# make a mosaic plot with an informative title and axes labels
mosaicplot(species, xlab = "population stauts", ylab = "legal protection",
           main = "Legal protection impacts on populations")

# set up contingency table following null hypothesis
species_null <- matrix(c(17,20,16,20), ncol=2, byrow = TRUE)
colnames(species_null) <- c("Not protected", "Protected")
rownames(species_null) <- c("Declining", "Stable/Increase")


###############################################################################
################################ Question 9 ###################################
###############################################################################

# Conduct a chi-squared test
chisq.test(species)


