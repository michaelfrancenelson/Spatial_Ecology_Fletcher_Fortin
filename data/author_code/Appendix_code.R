########################################################
########################################################
#Fletcher and Fortin 2018
#Appendix: Introduction to R
########################################################
########################################################

######################
#Getting help in R
######################

help(package="MASS")    #package help
?glm                    #function help
args(glm)               #provides arguments and defaults for functions

#######################################
#R Classes
#######################################

x <- 4
x = 7

#making a vector
x.vector <- vector(mode = "numeric", length = 4)
x.vector

x.vector <- 1:10
x.vector

x.vector <- c(0, 4, 0, 1)
x.vector

#making a matrix
x.matrix <- matrix(0, nrow = 5, ncol = 5)
x.matrix

#making an array
x.array <- array(0, dim = c(5, 5, 2))
x.array

#making a dataframe with two columns
x.df <- data.frame(site = c("North", "South", "Mid"), x = 1:3, y = NA)
x.df

#accessing data
x.vector
x.vector[2]
x.vector[1:3]

x.matrix
x.matrix[2,2] <- 5
x.matrix[,2] <- 2
x.matrix[3,] <- 3
diag(x.matrix) <- 1
x.matrix

#making a list, that is a combination of data objects already defined
x.list <- list(x.vector, x.matrix)

x.list
x.list[[1]]
x.list[[2]]
x.list[[2]][1,2]

###########################################
#Getting Data Into and Out of R
###########################################

#Set working directory manually
setwd('C:\\Spatial Ecology Book\\Appendix')     #your directory will change (this one is mine)

#or
setwd('C:/Spatial Ecology Book/Appendix')

#set working directory with choose.dir
setwd(choose.dir())

#Set working directory with pull-down menus in RStudio:
#  >Tools>Set working directory>Choose directory

#Importing data from a txt or csv file:
landbird <- read.csv("vath_2004.csv", header = TRUE)
landbird_cov <- read.csv("vath_covariates.csv", header = T)

#OR with file choose:
landbird <- read.table(file.choose(), header = T)

#to export data/results
write.table(x.array, file = "array.txt", sep = " ")
write.csv(x.array, file = "array_export.csv")

######################################
#Installing packages
######################################

#need to set CRAN Mirror first
#then install from CRAN
install.packages("lme4")

#load package
library(MASS)

#installing from a zip file downloaded onto desktop in RStudio
# >Tools>Install Packages>Install From>Package Archive File>

#remove package
detach("package:MASS")

#Installing an old version of R or a package from a tar file (here, downloaded on your desktop)
install.packages("C:\\Users\\Rob Fletcher\\Desktop\\R-2.14.2.tar.gz", repos = NULL, type = "source")

##############################
#Functions in R
##############################

arith.mean <- function(x) sum(x) / length(x)

arith.mean(x.vector)
mean(x.vector)

#a slightly more complex function to calculate standard error of the mean
stderr <- function(x){
  N <- length(x)
  stderr <- sd(x)/(N^0.5)
  return(stderr)
}
stderr(x.vector)

###################################################
#Data access, management and manipulation in R
###################################################

#----------------------#
#Accessing data
#----------------------#

#To view the objects in the workspace
ls()

names(landbird)

#view the first few rows/or last few rows
head(landbird)
tail(landbird, n = 10)

#accessing column 1 of data
landbird[ , 1]
landbird[,"SURVEYID"]

#accessing row 1 of data
landbird[1, ]

#accessing a subset of data: observations where VATH occurred
landbird[landbird$VATH > 0,]
subset(landbird, VATH > 0)

#accessing a subset of data: a portion of one variable based on another
landbird$VATH[landbird$NORTHING < median(landbird$NORTHING)]

#number of dimensions
dim(landbird)

#number of unique values in the variable Rep
unique(landbird$Rep)

#the number of levels in a factor variable
levels(landbird$Rep)
levels(as.factor(landbird$Rep))

#Understand how R is interpreting the variables (e.g., are they continuous or categorical?)
str(landbird)

#-------------------------------#
#Merging, appending, removing
#-------------------------------#

landbird <- merge(landbird, landbird_cov, by = "SURVEYID", all = TRUE)

names(landbird)

#-------------------------------#
#Data subsetting and summaries
#-------------------------------#

#Some common functions summarizing data, applied to density of least flycatchers (LEFL)

median(landbird$Elev)

mean(landbird$Elev)

sd(landbird$Elev)                                            #standard deviation

length(landbird$Elev)                                        #number of observations

quantile(landbird$Elev, prob=0.95)                           #upper 95% quantile

#Common summary statistics for each column in df
summary(landbird)

#Use functions on a subset of the data with tapply
tapply(landbird$Elev, landbird$VATH, mean)                   #mean
tapply(X = landbird$Elev, INDEX = landbird$VATH, FUN = sd)   #standard deviation

#using plyr
install.packages("plyr", dependencies = T)
library(plyr)

#summarize by VATH pres/abs
ddply(landbird, .(VATH),
      summarize,
      elev.mean = mean(Elev),
      elev.sd = sd(Elev))

#summarize by VATH pres/abs and mesic
ddply(landbird, .(VATH,Mesic),
      summarize,
      elev.mean = mean(Elev),
      elev.sd = sd(Elev))

#summaries without specifying columns
ddply(landbird, .(VATH,Mesic), numcolwise(mean), na.rm=T)

#with dplyr
library(dplyr)

landbird %>%                           #pipe
  group_by(VATH,Mesic) %>%             #categories for grouping
  summarise(elev.mean = mean(Elev),
            elev.sd = sd(Elev))

###################################
#DATA MANIPULATION
###################################

#subsetting data, by rows
landbird.pres <- landbird[landbird$VATH==1, ]

#or using subset
landbird.pres2<- subset(landbird, VATH==1)

#subset based on 2 criteria
landbird.pres.mesic <- subset(landbird, VATH==1 & Mesic==1)

#make a new object of vegetation by combining columns(cbind)
coordinates <- landbird[,cbind(4:5)]###this combines columns 4,5

#or
coordinates <- landbird[,cbind("EASTING", "NORTHING")]###this combines columns 4,5

#make a new object by removing columns none of the birds by removing columns
sites <- landbird[,cbind(-4:-8)] ###this new object contain all columns except the columns 1-3.

#--------------------------------------#
#Reformatting data
#--------------------------------------#

#Convert elevation to high versus low
elev.median <- median(landbird$Elev)

landbird$Elev_cat <- "low"
landbird$Elev_cat[landbird$Elev>elev.median] <- "high"

#or
landbird$Elev_cat2 <- as.factor(ifelse(landbird$Elev > elev.median, "high", "low"))

#restructing data with reshape2
install.packages("reshape2",dependencies=T)
library(reshape2)

#long to wide format with dcast
transect.VATH <- dcast(landbird, TRANSECT ~ Rep, value.var = "VATH")

head(transect.VATH)

#wide to long format with melt
transect.long.VATH <- melt(transect.VATH, id.vars = "TRANSECT", variable.name = "Rep",
                         value.name = "VATH")
head(transect.long.VATH)

#remove NAs
transect.long.VATH <- transect.long.VATH[complete.cases(transect.long.VATH[,3]),]
nrow(transect.long.VATH)
nrow(landbird)

#rowSums
names(transect.VATH)
transect.freq <- rowSums(transect.VATH[,2:11])
transect.freq <- rowSums(transect.VATH[,2:11], na.rm = T)         #remove NAs in calculations

#summing rows with apply
apply(transect.VATH[,2:11],1, sum, na.rm=T)

######################################
#Graphics in R
######################################

#this is ridiculously brief: see the many books on R graphics

#-------------------------------#
#Scatterplots
#-------------------------------#

plot(landbird$EASTING, landbird$NORTHING)

#Add some axes labels
plot(landbird$EASTING, landbird$NORTHING, xlab = "Easting (UTMs)", ylab = "Northing (UTMs)")

#multiple panels
plot(landbird[,c(4:5,7)])
pairs(landbird[ ,c(4:5,7)])

#-------------------------------#
#boxplot
#-------------------------------#

boxplot(landbird$Elev)

#-------------------------------#
#histogram
#-------------------------------#

hist(landbird$Elev, xlab = "Elev", main = "")

###############################################################
#Spatial data in R
###############################################################

install.packages("raster", dependencies = T)      #should include sp too
install.packages("rgdal", dependencies = T)       #for coordinate systems

library(sp)
library(raster)
library(rgdal)#for projections/loading vector files

#---------------------------#
#Using spatial classes
#---------------------------#

###Spatial points###

points.coord <- cbind(landbird$EASTING, landbird$NORTHING)
points.attributes <- data.frame(transect = landbird$TRANSECT, point = landbird$Rep, VATH = landbird$VATH)

#create spatial points data frame
points.spdf <- SpatialPointsDataFrame(points.coord, data = points.attributes)
plot(points.spdf)

coordinates(points.spdf)
head(points.spdf)
names(points.spdf)

#subset
points.spdf.pres <- points.spdf[points.spdf$VATH==1,]

plot(points.spdf)
plot(points.spdf.pres)

###Spatial Lines###

#create from points on transects; a list of lines
points.lines <- lapply(split(points.spdf, points.spdf$transect),
                function(x) Lines(list(Line(coordinates(x))), x$transect[1L]))
str(points.lines)
points.sl<- SpatialLines(points.lines)

#add line attributes
transect.attributes <- data.frame(transect = unique(landbird$TRANSECT))
rownames(transect.attributes) <- transect.attributes$transect
points.sldf <- SpatialLinesDataFrame(points.sl, data = transect.attributes)

#plot
plot(points.sldf)
head(points.sldf)
str(points.sldf)

#subset one transect for plotting
points.sldf1 <- points.sldf@lines[[1]]
str(points.sldf1)

#plot
plot(points.sldf1@Lines[[1]]@coords[])
lines(points.sldf1@Lines[[1]]@coords)

###Spatial polygons###

#watersheds of the region (huc_level8 from USGS/USDA)
watersheds <- readOGR("water")          #watershed folder, with .shp inside

head(watersheds, 6)
summary(watersheds)
dim(watersheds)#63 watersheds

#plot
plot(watersheds, col = "gray")
invisible(text(getSpPPolygonsLabptSlots(watersheds), labels = as.character(watersheds$NAME), cex=0.4))

#subset and plot one watershed
huc_bitterroot <- watersheds[watersheds$NAME=="Bitterroot",]
plot(huc_bitterroot)
plot(points.spdf, col = "red", add = T)

#write shp files to folder
writeOGR(watersheds2, dsn = "water", layer = "watersheds", driver = "ESRI Shapefile")

###raster grids

elev <- raster("elev.gri")     #elevation layer
elev

#plot
plot(elev)

#add spatial points
plot(points.spdf, add = T)

#characteristics of layer
res(elev)
extent(elev)
dim(elev)
ncell(elev)
summary(elev)

#add another raster
mesic <- raster("mesic.gri") #mesic forest layer
plot(mesic)

#check to see if we can combine rasters
compareRaster(elev, mesic)

#create raster stack
layers <- stack(elev, mesic)
plot(layers)

#summarize
inMemory(elev)

#zonal stats by watershed (a bit slow: 3 min on my laptop)
system.time(watershed.raster <- rasterize(watersheds, elev, field = 1:nrow(watersheds)))
plot(watershed.raster, col = terrain.colors(100))

#plot
plot(watersheds)
invisible(text(getSpPPolygonsLabptSlots(watersheds), labels = 1:nrow(watersheds), cex = 0.7))

#zonal summaries: mean elevation by watershed
watershed.zone <- zonal(elev, z = watershed.raster, mean)
watershed.zone

###########################################
#Projections and transformations
###########################################

#-----------------------------------------#
#Coordinate system info for CRS:
#Albers Conical Equal Area
#metadata at: https://www.fs.usda.gov/wps/portal/fsinternet/cs/detail/!ut/p/z0/04_Sj9CPykssy0xPLMnMz0vMAfIjo8zijQwgwNHCwN_DI8zPyBcqYKBfkO2oCABZcx5g/?position=Not%20Yet%20Determined.Html&pname=Region%201%20-%20Geospatial%20Data&ss=1101&navtype=BROWSEBYSUBJECT&pnavid=160000000000000&navid=160130000000000&ttype=detail&cid=fsp5_030930
#  NAD83 Unit of measure: meters
#  Projection: Albers Conical Equal Area
#  Albers Central Meridian: -109.5
#  Standard Parallel 1: 46
#  Standard Parallel 2: 48
#  Latitude of Origin: 44
#  False Easting: 600000
#  False Northing: 0
#-----------------------------------------#

#define a latlong, WGS84 projection for the coordinate reference system (CRS)
crs.layers <- CRS("+proj=aea +lat_1=46 +lat_2=48
                   +lat_0=44 +lon_0=-109.5 +x_0=600000 +y_0=0
                   +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

crs.latlong <- CRS("+proj=longlat +ellps=WGS84")

proj4string(points.spdf)
proj4string(points.spdf) <- crs.layers
proj4string(points.spdf)

coordinates(points.spdf)

points.spdf <- spTransform(points.spdf, crs.latlong)
coordinates(points.spdf)

