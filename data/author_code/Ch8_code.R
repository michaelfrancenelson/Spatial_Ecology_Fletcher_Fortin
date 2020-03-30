########################################################
########################################################
#Fletcher and Fortin 2019
#Chapter 8: Space Use and Resource Selection
########################################################
########################################################

#load packages

library(raster)           #for raster covariate data; version 2.6-7 used
library(reshape2)         #for re-formatting data; version 1.4.3 used
library(rgdal)            #for reading different types of GIS files; version 1.3-4 used
library(adehabitatLT)     #for trajectory information; version 0.3.23 used
library(adehabitatHR)     #Home range estimation; version 0.4.15 used
library(adehabitatHS)     #for selection ratios; version 0.3.13 used
library(survival)         #for conditional logit model; version 2.42-3 used

#set working directory where data were downloaded
setwd(choose.dir())

###################################################
#8.3.2 Prepping the data
###################################################

#landcover source: fwc/fnai
land <- raster("panther_landcover")

#check projection
projection(land)

#Add panther data
panthers <- readOGR("panthers.shp")

#check projection
projection(panthers)

#label projection for later use
crs.land <- projection(land)

#inspect
summary(panthers)
levels(panthers$CatID) #the unique cat IDs
head(panthers)

#plot
plot(land)
points(panthers, col=panthers$CatID)

#load reclassification table for reclassifying map
classification <- read.table("landcover reclass.txt", header=TRUE)

#inspect
head(classification)
levels(classification$Description)    #original classification
levels(classification$Description2)   #re-class

#format for reclassify function;
class <- as.matrix(classification[,c(1,3)])
land_sub <- reclassify(land,rcl=class)

#plot
plot(land_sub)

#create forested wetlands layer
wetforest <- land_sub
values(wetforest) <- 0
wetforest[land_sub==9 | land_sub==11] <- 1

#create forested uplands layer
dryforest <- land_sub
values(dryforest) <- 0
dryforest[land_sub==10 | land_sub==12] <- 1

#5 km moving window to get neighborhood proportion
fw <- focalWeight(land_sub, 5000, 'circle')
dry.focal <- focal(dryforest, w=fw, fun="sum", na.rm=T)
wet.focal <- focal(wetforest, w=fw, fun="sum", na.rm=T)

#merge into a single raster stack
layers <- stack(land_sub, wet.focal, dry.focal)
names(layers) <- c("landcover", "wetforest", "dryforest")

#plot
plot(layers)

###################################################
#8.3.3 Home range analysis
###################################################

#------------------#
#mcp home range
#------------------#

mcp95 <- mcp(panthers[,"CatID"], percent = 95)
mcp50 <- mcp(panthers[,"CatID"], percent = 50)

#inspect
class(mcp95)
head(mcp95@polygons)

#plot
plot(land_sub)
plot(panthers, add=TRUE, col=panthers$CatID)
plot(mcp95, add=TRUE)
plot(mcp50, add=TRUE, col="orange")

#------------------------------------#
#fixed bivariate kernel home range
#------------------------------------#

#kernel types:
kernel.href.bivar <- kernelUD(panthers[,"CatID"], h="href", kern="bivnorm")
kernel.href.epa <- kernelUD(panthers[,"CatID"], h="href", kern="epa")

#plot
image(kernel.href.bivar)
image(kernel.href.epa)

#alternative plot for first cat
plot(kernel.href.bivar[[1]])
plot(kernel.href.epa[[1]])

#UD data
kernel.href.bivar[[1]]@data

#h value for bandwidth
kernel.href.bivar[[2]]@h
kernel.href.bivar[[2]]@h$h

#least-squares cross validation for h
kernel.lscv.bivar <- kernelUD(panthers[,"CatID"], h="LSCV", kern="bivnorm")

#manually adjust h
kernel.bivar.h1000 <- kernelUD(panthers[,"CatID"], h=1000, kern="bivnorm")

#plot first cat
plot(kernel.bivar.h1000[[1]])
plot(kernel.href.bivar[[1]])

#contour maps of activity for fourth cat
plot(kernel.href.bivar[[4]])
contour.data <- as.image.SpatialGridDataFrame(kernel.href.bivar[[4]])
contour(contour.data, nlevels=5, add=TRUE)

#-------------------------------#
#local convex hull home range
#-------------------------------#

#subset
panther147 <- panthers[panthers$CatID==147, ]
panther100 <- panthers[panthers$CatID==100, ]

#plot
plot(panther147)
coordinates(panther147)

#initialize
k.int <- round(nrow(coordinates(panther147))^0.5,0)
a.int <- round(max(dist(coordinates(panther147))),0)

k.search <- seq(k.int, 10*k.int, by=5)
a.search <- seq(a.int, 2*a.int, by=3000)

#Parameter search for locoh-a
LoCoH.a.range <- LoCoH.a.area(SpatialPoints(coordinates(panther147)), unout="km2", arange=a.search)

#Parameter search for locoh-k
LoCoH.k.range <- LoCoH.k.area(SpatialPoints(coordinates(panther147)), unout="km2", krange=k.search)

#plot
plot(LoCoH.a.range)
plot(LoCoH.k.range)

#inspect
a.search[5]
k.search[11]

#re-fit model
LoCoH.k.61 <- LoCoH.k(SpatialPoints(coordinates(panther147)), k=k.search[11])

#plot
plot(LoCoH.k.61)

#re-fit model
LoCoH.a.100062 <- LoCoH.a(SpatialPoints(coordinates(panther147)), a=a.search[5])

#plot
plot(LoCoH.a.100062)

#-----------------------------#
#brownian bridge home range
#-----------------------------#

#Re-format Juldate information:
#function for taking characters of a string from rightmost value
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#re-format
panthers$Juldate <- as.character(panthers$Juldate)
panther.date <- as.numeric(substrRight(panthers$Juldate, 3))
panthers$Date<-as.Date(panther.date, origin=as.Date("2006-01-01"))

#convert to POSIXct object
panthers$Date <- as.POSIXct(panthers$Date,"%Y-%m-%d", tz = "EST")

#convert to data frame
panther.df <- as.data.frame(panthers)

#make trajectory object
panther.ltraj <- as.ltraj(xy=coordinates(panthers), date=panthers$Date, id=panthers$CatID, typeII=T)

#inspect
head(panther.ltraj)
head(panther.ltraj[[1]], 2)
summary(panther.ltraj)

#plot
plot(panther.ltraj)

#telemetry error (average)
sigma2 <- 450

#estimate sigma1
sigma1 <- liker(panther.ltraj, sig2 = sigma2, rangesig1 = c(2, 100))

#inspect
sigma1
str(sigma1)

#brownian bridge for Cat 147
bb.147 <- kernelbb(panther.ltraj[6], sig1 = 7.2, sig2 = sigma2, grid = 200)

#all panthers
sig1 <- c(sigma1[[1]]$sig1, sigma1[[2]]$sig1, sigma1[[3]]$sig1, sigma1[[4]]$sig1, sigma1[[5]]$sig1, sigma1[[6]]$sig1)
bb.panther <- kernelbb(panther.ltraj, sig1 = sig1, sig2 = sigma2, grid = 200)

#plot
plot(panther.ltraj[6])
plot(bb.147)

#----------------------------#
#contrast estimates
#----------------------------#

#home range area estimates
kernel.95 <- getverticeshr(kernel.href.bivar, percent=95)
bb.95 <- getverticeshr(bb.panther, percent=95)

#contrast area
mcp95$area
kernel.95$area
bb.95$area

#plot
par(mfrow=c(1,2))  #sets up the graph window to store two graphs
plot(land_sub)
plot(kernel.95, add=TRUE, col=kernel.95$id)

plot(land_sub)
plot(mcp95, add=TRUE, col=kernel.95$id)
dev.off()

#write to shapefile if needed
writePolyShape(mcp95, "homerange")

########################################################
#8.3.4 Resource Selection
########################################################

###########################################
#8.3.4.1 Point selection functions
###########################################

#use data
use <- extract(layers, panthers)
use <- data.frame(use)

#inspect
head(use)
str(use)

#add CatID
use$CatID <- as.factor(panthers$CatID)

#reformat
useCatID <- dcast(use, CatID~landcover, length, value.var="CatID")

#inspect
useCatID

#add land-cover names
newclass.names <- unique(classification[,3:4])
names(useCatID) <- c("CatID", as.character(newclass.names[1:13,2]))

#inspect
useCatID

#---------------------------------------------------#
#design II availability: population availability
#---------------------------------------------------#

#get availability points
set.seed(8)
rand.II <- sampleRandom(layers, size=1000)
rand.II <- data.frame(rand.II)


#inspect
head(rand.II)
str(rand.II)

rand.II.land <- as.factor(rand.II$landcover)

#get counts of each landcover type
avail.II <- tapply(rand.II.land, rand.II.land, length)

#inspect
avail.II

#add land-cover names
names(avail.II) <- as.character(newclass.names[1:14,2])

#inspect
avail.II

#remove exotics, which were not observed in use sample
avail.II <- avail.II[c(-14)]

#--------------------------------------------------------------------------#
#design III availability: within home-range availability for each individual
#--------------------------------------------------------------------------#

cat.unique <- unique(panthers$CatID)
samples <- 200
rand.III <- matrix(nrow=0, ncol=4)

#loop for all individuals
for(i in 1:length(cat.unique)){

  id.i <- cat.unique[i]
  cat.i <- panthers[panthers$CatID==id.i,]
  mcp.i <- mcp(SpatialPoints(coordinates(cat.i)), percent = 99)
  rand.i <- spsample(mcp.i, type="random", n=samples)
  rand.i.sample <- extract(layers, rand.i)

  #make a matrix of CatID and rand samples
  cat.i <- rep(cat.unique[i], length(rand.i))
  rand.cat.i <- cbind(cat.i, rand.i.sample)
  rand.III <- rbind(rand.III, rand.cat.i)
}

#inspect
head(rand.III)
class(rand.III)
str(rand.III)

#reshape data
rand.III <- data.frame(rand.III)
rand.III$cat.i <- as.factor(rand.III$cat.i)
avail.III <- dcast(rand.III, cat.i~landcover, length, value.var="cat.i")

#inspect
avail.III

#---------------------------------------#
#selection ratios
#---------------------------------------#

#Design II:
sel.ratioII <- widesII(u=useCatID[,c(2:ncol(useCatID))], a=as.vector(avail.II), avknown=FALSE, alpha = 0.05)

#inspect
sel.ratioII
sel.ratioII$wi
sel.ratioII$se.wi

#plot
plot(sel.ratioII)

#Design III:
sel.ratioIII <- widesIII(u=useCatID[,c(2:ncol(useCatID))], a=avail.III[,2:14], avknown=FALSE, alpha = 0.05)

#inspect
sel.ratioIII
sel.ratioIII$wi
sel.ratioIII$se.wi
sel.ratioIII$ICwiupper
sel.ratioIII$ICwilower

#plot
plot(sel.ratioIII)

#---------------------------------------#
#logistic rsf
#---------------------------------------#

#create data frame
use.cov <- data.frame(use[,1:3], use=1)
back.cov <- data.frame(rand.II, use=0)
all.cov <- data.frame(rbind(use.cov, back.cov))

#inspect
str(all.cov)

#convert landcover to factor
all.cov$landcover <- as.factor(all.cov$landcover)

#run logistic rsf
rsf.all <- glm(use ~ landcover + wetforest + dryforest, family=binomial(link=logit), data=all.cov)
rsf.forest <- glm(use ~ wetforest + dryforest, family=binomial(link=logit), data=all.cov)

#inspect
summary(rsf.forest)
summary(rsf.all)

#likelihood ratio-test for land-cover
anova(rsf.forest, rsf.all, test="LRT")

#-------------------------------------------#
#inhomogeneous point process
#-------------------------------------------#

#get polygon boundary for study area
raster.extent <- land > -Inf
studyregion <- rasterToPolygons(raster.extent, dissolve=TRUE)

#plot
plot(studyregion)

#create regular grid
rand.grid <- spsample(studyregion, cellsize = 1000, type="regular")
grid.1km <- SpatialPoints(rand.grid, proj4string = CRS(crs.land))
grid.area <- 1000 * 1000

#extract covariates
rand.cov.grid <- extract(layers, grid.1km)
use.cov.grid <- data.frame(use[,1:3], use = 1, grid.area = 1)
back.cov.grid <- data.frame(rand.cov.grid, use = 0, grid.area = grid.area)
all.cov.grid <- data.frame(rbind(use.cov.grid, back.cov.grid))

#logistic approximation to ipp
rsf.ipp.forest <- glm(use ~ wetforest + dryforest, weight = grid.area,
                    family = binomial(link = logit),data = all.cov.grid)

#inspect and compare
summary(rsf.ipp.forest)
summary(rsf.forest)

###########################################
#8.3.4.2 Step selection functions
###########################################

#plot trajectories
plot(panther.ltraj)
plot(panther.ltraj, id="147")

#inspect
head(panther.ltraj[[1]])

#step length for second CatID
panther.ltraj[[2]][,6]

#plot
hist(panther.ltraj[[2]][,6], main="Second CatID")

#plots of relative movement angles
rose.diag(na.omit(panther.ltraj[[2]][,10]), bins=12, prop=1.5)
circ.plot(panther.ltraj[[2]][,10], pch=1)

#step data
stepdata <- data.frame(coordinates(panthers))
stepdata$CatID <- as.factor(panthers$CatID)
names(stepdata) <- c("X","Y","CatID")

#generate 3 random steps from empirical distribution for each observation
n.use <- dim(stepdata)[[1]]
n.avail <- n.use*3

#convert back to data frame for easy manipulation
traj.df <- ld(panther.ltraj)

#sample with replacement
avail.dist <- matrix(sample(na.omit(traj.df$dist), size=n.avail, replace=TRUE), ncol=3)
avail.angle <- matrix(sample(na.omit(traj.df$rel.angle), size=n.avail, replace=TRUE), ncol=3)

#inspect
head(avail.dist)
head(avail.angle)

#add names
colnames(avail.dist) <- c("a.dist1", "a.dist2", "a.dist3")
colnames(avail.angle) <- c("a.angle1", "a.angle2", "a.angle3")

#link available data to observations
traj.df <- cbind(traj.df, avail.dist, avail.angle)

#inspect
head(traj.df)

#plot and compare
hist(traj.df[,6], main="distance data")
hist(avail.dist, main="Avail distance")

#calculate new coordinates of available points from dist/angles
#to cartesian from polar coords

#check for one point:
traj.df[2, "x"] + traj.df[2, "dist"]*cos(traj.df[2, "abs.angle"])#new x
traj.df[2, "y"] + traj.df[2, "dist"]*sin(traj.df[2, "abs.angle"])#new y
traj.df[3, c("x", "y")]

#calculate new coordinates in t+1 from observed t using rel angle (need previous angle):

#check for one point:
traj.df[2, "x"] + traj.df[2, "dist"]*cos(traj.df[1, "abs.angle"] + traj.df[2, "rel.angle"])#new x
traj.df[2, "y"] + traj.df[2, "dist"]*sin(traj.df[1, "abs.angle"] + traj.df[2, "rel.angle"])#new y
traj.df[3,c("x", "y")]

#calculate available coords for all data
#first add column of abs.angle for t-1
traj.df$abs.angle_t_1 <- NA
for(i in 2:nrow(traj.df)){
  traj.df$abs.angle_t_1[i] <- ifelse(traj.df$id[i]==traj.df$id[i-1], traj.df$abs.angle[i-1], NA)
  }

#alternative without for loop
#traj.df$abs.angle_t_1 <- c(NA,traj.df$abs.angle[1:nrow(traj.df)-1])
#traj.df[!duplicated(traj.df$id), "abs.angle_t_1"] <- NA

#calculate use coords for t+1
traj.df$x_t1 <- traj.df[,"x"] + traj.df[,"dist"] * cos(traj.df[,"abs.angle"])
traj.df$y_t1 <- traj.df[,"y"] + traj.df[,"dist"] * sin(traj.df[,"abs.angle"])

#calculate avail coords for t+1
traj.df$x_a1 <- traj.df[,"x"] + traj.df[,"a.dist1"] * cos(traj.df[,"abs.angle_t_1"] + traj.df[,"a.angle1"])
traj.df$y_a1 <- traj.df[,"y"] + traj.df[,"a.dist1"] * sin(traj.df[,"abs.angle_t_1"] + traj.df[,"a.angle1"])

traj.df$x_a2 <- traj.df[,"x"] + traj.df[,"a.dist2"] * cos(traj.df[,"abs.angle_t_1"] + traj.df[,"a.angle2"])
traj.df$y_a2 <- traj.df[,"y"] + traj.df[,"a.dist2"] * sin(traj.df[,"abs.angle_t_1"] + traj.df[,"a.angle2"])

traj.df$x_a3 <- traj.df[,"x"] + traj.df[,"a.dist3"] * cos(traj.df[,"abs.angle_t_1"] + traj.df[,"a.angle3"])
traj.df$y_a3 <- traj.df[,"y"] + traj.df[,"a.dist3"] * sin(traj.df[,"abs.angle_t_1"] + traj.df[,"a.angle3"])

#reformat data for step selection
traj.df <- traj.df[complete.cases(traj.df),]#remove NAs

traj.use <- data.frame(use=rep(1, nrow(traj.df)), traj.df[,c("id", "pkey", "date", "x_t1", "y_t1")])
traj.a1  <- data.frame(use=rep(0, nrow(traj.df)), traj.df[,c("id", "pkey", "date", "x_a1", "y_a1")])
traj.a2  <- data.frame(use=rep(0, nrow(traj.df)), traj.df[,c("id", "pkey", "date", "x_a2", "y_a2")])
traj.a3  <- data.frame(use=rep(0, nrow(traj.df)), traj.df[,c("id", "pkey", "date", "x_a3", "y_a3")])

names(traj.use) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a1) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a2) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a3) <- c("use", "id", "pair", "date", "x", "y")

#append use and available data together
stepdata.final <- rbind(traj.use, traj.a1, traj.a2, traj.a3)

#inspect
head(stepdata.final)

#-------------------------------------#
#get covariates
#-------------------------------------#

#create a spatial points dataframe
step.coords <- SpatialPoints(stepdata.final[,c("x", "y")])
projection(step.coords) <- crs.land

#plot
plot(land_sub)
points(step.coords)

#extracts covariates
cov <- extract(layers, step.coords)

#add covariates to dataframe
stepdata.final <- data.frame(cbind(stepdata.final, cov))

#inspect
head(stepdata.final)

#------------------------------------#
#FIT CONDITIONAL LOGISTIC REGRESSION
#------------------------------------#

#conditional logit model
logit.ssf <- clogit(use ~ wetforest + dryforest + strata(pair), data=stepdata.final)

#inspect
summary(logit.ssf)
confint(logit.ssf)

#conditional logit, including catID as cluster
logit.cat.ssf <- clogit(use ~ wetforest + dryforest + strata(pair) + cluster(id),
                        method="approximate", data=stepdata.final)

#inspect
summary(logit.cat.ssf)
confint(logit.cat.ssf)

#logistic rsf, ignoring the local pairing structure of the data
logit.rsf <- glm(use ~ wetforest + dryforest, family="binomial", data=stepdata.final)

#inspect
summary(logit.rsf)
confint(logit.rsf)

###########################################
#8.3.4.3 Path selection functions
###########################################

#consider one panther
panther147.traj <- panther.ltraj[6]

#plot
plot(panther147.traj)

#random shift of trajectory
path.model <- NMs.randomShiftRotation(panther147.traj, rshift=F, rrot=T, nrep=1)

#simulate
path.avail <- testNM(path.model)

#inspect
str(path.avail)

#reformat for plotting
path.avail.df <- data.frame(path.avail[[1]])
path.avail.ltraj  <- as.ltraj(xy=path.avail.df[,c("x", "y")], date=path.avail.df[,"date"],
                              id=rep(147, nrow(path.avail.df)))

#plot to compare
plot(path.avail.ltraj)
plot(panther.ltraj, id="147")

#CRW model
CRW.model <- NMs.randomCRW(panther147.traj, rangles=T, rdist=T, nrep=1)
CRW.avail <- testNM(CRW.model)

#reformat for plotting
CRW.avail.df <- data.frame(CRW.avail[[1]])
CRW.avail.ltraj  <- as.ltraj(xy=CRW.avail.df[,c("x", "y")], date=CRW.avail.df[,"date"], id=rep(147, nrow(CRW.avail.df)))

#plot
par(mfrow=c(1, 3))
plot(panther.ltraj, id="147", main="observed")
plot(path.avail.ltraj, main="Rotated path")
plot(CRW.avail.ltraj, main="CRW")
dev.off()
