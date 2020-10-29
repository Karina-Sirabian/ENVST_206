# Activity 8
# 10/23/20

# Install Packages
install.packages(c("raster"))
library(raster)
library(ggplot2)
library(rgdal)

# set up directory for oneida data folder
dirR <- "/Users/russell/GitHub/ENVST_206/Activity_8/a08/oneida"

# read in Sentinel data
rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

# Plot blue light reflected over Oneida Lake Area
plot(rdatB2/10000)

# Stack red green blue
rgbS <- stack(rdatB4, rdatB3, rdatB2)/10000
# View raster, a few pixels in blue have reflectence above 1 so set scale
plotRGB(rgbS, scale = 2)
# Don't need to scale the argument when adding in the contrast stretch
plotRGB(rgbS, stretch = "lin")
# full resolutions
# get the total number of pixels by multiplying the number of rows and columns
# in the raster
plotRGB(rgbS, stretch="lin", maxpixels=rgbS@nrows*rgbS@ncols)
num_cells = rgbS@nrows*rgbS@ncols
num_cells
dim(rgbS)

# Make new raster stack with NIR in it
rgbS_new <- stack(rdatB4, rdatB3, rdatB2, rdatB8)/10000
# False Color Plot
plotRGB(rgbS_new, r = 4, g = 1, b = 2, stretch="lin", maxpixels=rgbS@nrows*rgbS@ncols)

# calculate NDVI
# NIR-red/(NIR + RED)
NDVI <- (rdatB8 - rdatB4)/ (rdatB8 + rdatB4)
# visualize NDVI across the Oneida lake area
plot(NDVI)

# Question 6 #
#read in landcover points data
#you may have to change your slash direction if you are on a windows computer
#I've also turned off the info print out here when you read in the file
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

#plot points and true color
plotRGB(rgbS, stretch="lin",maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19)
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19)
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19)
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19)
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19)
legend("bottomleft", c("algae","agri","forest","water","wetlands"),
       pch=19, col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75,0.5),rgb(0.33,0.33,0.65,0.5)),
       bty="n", cex=0.75)

#set up a dataframe with all of the point coordinates
landExtract <-  data.frame(landcID = as.factor(rep(c("algae","water","agri","forest","wetland"),each=120)),
                           x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords[,1]),
                           y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords[,2]))

#stack all bands
allbands <-  stack(rdatB2, rdatB3, rdatB4,rdatB8)/10000
#add the raster reflectance values to the point coordinates and classes
#extract(raster, matrix of coordinates)
#raster:: helps ensure that extract comes from the raster package
ExtractOut <- raster::extract(allbands,landExtract[,2:3])
#name the bands
colnames(ExtractOut) <- c("B02","B03","B04","B08")
#combine the original data with the coordinates with the raster data
rasterEx <- cbind(landExtract,ExtractOut)
#look at data
head(rasterEx)

ggplot(data=rasterEx, aes(x=B02, y=B03, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()

# Blue vs infrared
ggplot(data=rasterEx, aes(x=B02, y=B08, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()+
  ggtitle("Blue vs Near Infrared Reflectance")+
  theme(plot.title = element_text(hjust = 0.6, face = "bold"))

# Green vs infrared
ggplot(data=rasterEx, aes(x=B03, y=B08, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()+
  ggtitle("Green vs Near Infrared Reflectance")+
  theme(plot.title = element_text(hjust = 0.6, face = "bold"))

# Red vs infrared
ggplot(data=rasterEx, aes(x=B04, y=B08, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()+
  ggtitle("Red vs Near Infrared Reflectance")+
  theme(plot.title = element_text(hjust = 0.6, face = "bold"))



# Question 8 #
rasterEx
# Calculate NDVI for each point in rasterEx
rasterEx$NDVI <- (rasterEx$B08 - rasterEx$B04)/(rasterEx$B08 + rasterEx$B04)
# Create sub data frame with only Agriculture, forest, and wet land
rasterEx_sub <- filter(rasterEx, rasterEx$landcID == "agri")

rasterEx_sub <- rasterEx[rasterEx$landcID %in% c("agri", "forest", "wetland"),]

# Make Violin Plots
ggplot(data = rasterEx_sub, aes(x=landcID, y=NDVI))+ 
  geom_violin(fill=rgb(0.933,0.953,0.98))+ 
  geom_boxplot(width=0.06,size=0.25, fill="royalblue3")+ 
  theme_classic() +
  ggtitle("NDVI of Different Land Cover Types")+
  labs(x = "Land Cover Type", y = "Normalized Difference Vegetation Index (NDVI)")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_y_continuous(breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))+
  scale_x_discrete(labels = c("Agricultural Fields", "Forest", "Wetland"))


# Question 9
# specify model for NDVI and land cover type
in.mod <- lm(rasterEx_sub$NDVI ~ rasterEx_sub$landcID)
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
plot(tukeyT)

# Calculate means across factor data simultaneously
tapply(rasterEx_sub$NDVI, rasterEx_sub$landcID, "mean")






