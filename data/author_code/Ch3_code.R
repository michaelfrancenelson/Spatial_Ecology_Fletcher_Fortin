########################################################
########################################################
#Fletcher and Fortin 2019
#Chapter 3: Land-cover pattern and change
########################################################
########################################################

#load packages
library(raster)            #for raster data; version 2.6-7 used
library(rasterVis)         #for plotting rasters;  version 0.45 used
library(rgdal)             #for raster data, projections; version 1.3-4 used
library(SDMTools)          #for most landscape metrics; version 1.1-221 used
library(rgeos)             #for some patch metrics; version 0.3-28 used
library(igraph)            #for some landscape metrics; version 1.2.2 used
library(Voss)              #for neutral landscapes; version 0.1-4 used
library(secr)              #for neutral landscapes; version 3.1.6 used

######################################
#3.3.3 Land-cover at different scales
######################################

#set working directory where you downloaded the data
setwd(choose.dir())

#load landscape data
nlcd <- raster("nlcd2011gv2sr")

#check projection/crs
proj4string(nlcd)

#grain and extent
res(nlcd)
extent(nlcd)

#nlcd categories
levels(nlcd)
unique(nlcd)

#------------------------------------------#
#land-cover type (nlcd original categories)
#1 = forest:41-43
#2 = developed:21-24
#3 = agriculture:81,82
#4 = herbaceous:71-74
#5 = open:51-52
#6 = wetland:90,95
#7 = water:11-12
#------------------------------------------#

#convert land-cover integers to factor levels (categories)
nlcd <- as.factor(nlcd)

#add names of categories to raster layer
land_cover <- levels(nlcd)[[1]]
land_cover[,"landcover"] <- c("forest","developed", "ag","grass","open","wetland")
levels(nlcd) <- land_cover

#plot
land_col <- c("green","orange","yellow","brown","white","blue")
plot(nlcd, legend = T, col = land_col)

#plot with rasterVis
levelplot(nlcd, col.regions=land_col, xlab="", ylab="")

#create a reclassification matrix
nlcd.cat <- unique(nlcd)
nlcd.cat.for <- c(1,0,0,0,0,0)

reclass.mat <- cbind(nlcd.cat,nlcd.cat.for)
reclass.mat#first col: orginal; second: change to

#forest binary layer from reclassification matrix
nlcd.forest <- reclassify(nlcd,reclass.mat)
plot(nlcd.forest)

############################################
#3.3.3.1 patch-level quantification
############################################

#4-neighbor rule
forest.patchID4 <- clump(nlcd.forest, directions=4)
plot(forest.patchID4)
cellStats(forest.patchID4, max)#number of patches identified

#8-neighbor rule
forest.patchID8 <- clump(nlcd.forest, directions=8)
plot(forest.patchID8)
cellStats(forest.patchID8, max)#number of patches identified

#Now, calculate patch-level metrics:
?PatchStat
for.pstat <- PatchStat(forest.patchID8, cellsize=res(nlcd.forest)[[1]])
names(for.pstat)

#number of patches
nrow(for.pstat)

#mean patch metrics
for.pstat.mean <- colMeans(for.pstat[,2:ncol(for.pstat)])
for.pstat.mean

#SD of patch metrics
for.pstat.sd <- apply(for.pstat[,2:ncol(for.pstat)],2,sd)
for.pstat.sd

hist(log(for.pstat$area))

#correlation matrix
round(cor(for.pstat[,2:12]),2)

#plot
pairs(for.pstat[,c("n.cell","n.core.cell", "n.edges.perimeter","n.edges.internal")])
pairs(for.pstat[,c("area","core.area","core.area.index", "perim.area.ratio", "shape.index")])
pairs(for.pstat[,2:12])#all, but hard to see

############################################
#3.3.3.2 Class-level quantification
############################################

?ClassStat

#calculation based on forest layer
for.cstat <- ClassStat(nlcd.forest, cellsize=res(nlcd.forest)[[1]])
for.cstat

#calculation based on nlcd layer (all land-cover types)
nlcd.cstat <- ClassStat(nlcd, cellsize=res(nlcd)[[1]])
head(nlcd.cstat)

#check against PatchStat calculations:

#mean patch size
for.cstat[for.cstat$class==1, "mean.patch.area"]
for.pstat.mean["area"]#mean patch size

#standard deviation of patch shape
for.cstat[for.cstat$class==1, "sd.shape.index"]
for.pstat.sd["shape.index"]

#correlation matrix
cor(nlcd.cstat[,2:5])#subset of metrics

#plot subset of metrics
pairs(nlcd.cstat[,2:5])

#-------------------------------------------------------#
#distance-related metrics not calculated in SDMTools
#-------------------------------------------------------#

#using patchIDs calculated above
forest.poly <- rasterToPolygons(forest.patchID8, dissolve=T)

#plot
plot(forest.poly)

#-----------------------------------#
#core area and edge distances
#-----------------------------------#

#manual calculation of core area
core.poly <- gBuffer(forest.poly, width = -100, byid = T)
core.area <- gArea(core.poly, byid=T)

#plot
plot(forest.poly, col="grey30")
plot(core.poly, col="grey80", add=T)

#calculate distance to edge with raster package
nlcd.forestNA <- nlcd.forest
nlcd.forestNA[nlcd.forestNA == 1] <- NA
forest.dist <- raster::distance(nlcd.forestNA)

#plot distance to forest edge
plot(forest.dist)

#centroids of polygons
forest.centroid <- gCentroid(forest.poly, byid=T)

#plot centroids
plot(forest.poly, col="grey80")
plot(forest.centroid, add=T)

#edge-edge distance matrix using rgeos package
edge.dist <- gDistance(forest.poly, byid=T)

#centroid distance matrix
cent.dist <- gDistance(forest.centroid, byid=T)

#patch-level nearest-neighbor distance
diag(cent.dist) <- NA
diag(edge.dist) <- NA

nnd.cent <- apply(cent.dist, 1, min, na.rm=T)
nnd.edge <- apply(edge.dist, 1, min, na.rm=T)

plot(nnd.cent,nnd.edge)
cor(nnd.cent,nnd.edge)

#-----------------------------------#
#proximity index
#-----------------------------------#

#patch area
patch.area <- data.frame(id=for.pstat$patchID, area=for.pstat$area)

#neighborhood for proximity index to be calculated
#NOTE: using 4-neighbor rule results in some 0 distances, which requires a correction in formula

h <- edge.dist
h2 <- 1/h^2
h2[edge.dist > 1000] <- 0
diag(h2) <- 0

#proximity index
patch.prox <- rowSums(sweep(h2, 2, patch.area$area, "*"))

#correlation of metrics
cor(cbind(patch.prox, area=patch.area$area, nnd.cent, nnd.edge))

############################################
#3.3.3.3 landscape-level quantification
############################################

#some summary metrics derived from class-level metrics

land.NP <- sum(nlcd.cstat$n.patches)
land.PD <- sum(nlcd.cstat$patch.density)
land.LPI <- max(nlcd.cstat$largest.patch.index)
land.TE <- sum(nlcd.cstat$total.edge)/2
land.ED <- sum(nlcd.cstat$edge.density)/2
land.AI <- sum(nlcd.cstat$prop.landscape*nlcd.cstat$aggregation.index)

#----------------------------------#
#some diversity-related metrics
#----------------------------------#

#richness
richness <- length(unique(values(nlcd)))
richness

#a function richness
richness <- function(x) length(unique(na.omit(x)))

#diversity,D, and evenness, E
table(values(nlcd))

C <- table(values(nlcd))
P <- C / sum(C)
D <- -sum(P * log(P))
E <- D/log(length(C))

#----------------------------------#
#contagion
#----------------------------------#

#from raster; identifies adjacent cells
adj <- adjacent(nlcd, 1:ncell(nlcd), directions=4, pairs=T, include=T)
head(adj, 2)

#contigency table of pairwise adjacencies; should include ii
N <- table(nlcd[adj[,1]], nlcd[adj[,2]])
N


contagion<-function(r){
  adj <- adjacent(r, 1:ncell(r), directions=4, pairs=T, include=T)
  N <- table(r[adj[,1]], r[adj[,2]])
  N <- unclass(N)
  Ni <- rowSums(N)
  Pj_i <- as.matrix(N/Ni)
  Pi <- as.vector(unclass(table(values(r)))/ncell(r))
  Pij <- Pi*Pj_i
  n <- length(Pi)
  contagion <- 1 + sum(Pij * log(Pij),na.rm=T)/(log(n^2+n)-log(2))#Ritters formula

  return(contagion)
}

#----------------------------------#
#percent like adjacencies
#----------------------------------#

PLADJ<-function(r){
  adj <- adjacent(r, 1:ncell(r), directions=4, include=F)
  N <- table(r[adj[,1]], r[adj[,2]])
  N <- unclass(N)
  PLADJ <- sum(diag(N))/sum(N)*100

  return(PLADJ)
}
contagion(nlcd)
PLADJ(nlcd)

###############################################
#3.3.3.4 Moving window analysis
###############################################

#focal buffer matrix for moving windows
buffer.radius <- 100
fw.100m <- focalWeight(nlcd, buffer.radius, 'circle')#buffer in CRS units
round(fw.100m, 3)

#re-scale weight matrix to 0,1
fw.100m <- ifelse(fw.100m > 0, 1, 0)
fw.100m

#weight matrix for a Gaussian kernel
round(focalWeight(nlcd, c(50,100), type = "Gaus"), 2)

#forest cover moving window; number of cells
forest.100m <- focal(nlcd.forest, w=fw.100m, fun="sum", na.rm=T)

#proportion of forest
forest.prop.100m <-forest.100m/sum(fw.100m,na.rm=T)#proportion: divide by buffer size
plot(forest.prop.100m)

#richness moving window (took 0.61s on my laptop)
rich.100m <- focal(nlcd, fw.100m, fun=richness)
plot(rich.100m)

#diversity moving window function
diversity <- function(landcover, radius) {

  n=length(unique(landcover))

  #Create focal weights matrix
  fw.i <- focalWeight(landcover, radius, type="circle")

  #Compute focal means of indicators
  D <- landcover
  values(D) <- 0
  log.i <- function(x) ifelse(x==0, 0, x*log(x))#log(p)*p

  #for each landcover, create a moving window map and make calculations
  for (i in 1:length(n)) {
    focal.i <- focal(landcover == i, fw.i)
    D <- D + calc(focal.i, log.i)#take log(p)*p
  }

  D <- calc(D, fun=function(x){x * -1})#take negative
  return(D)
}

#calculate diversity moving window
diversity.100m <- diversity(landcover=nlcd, radius=100)
plot(diversity.100m)

cor(values(diversity.100m), values(rich.100m), use="complete.obs")

################################################
#3.3.4 Neutral Landscapes
################################################

#neutral landscape dimensions
dimX <- 128
dimY <- 128

#------------------------------------#
#Simple random landscapes
#------------------------------------#

#simple random with 10% habitat
sr.30 <- raster(ncol=dimX, nrow=dimY, xmn=0, xmx=dimX, ymn=0, ymx=dimY)
sr.30[] <- rbinom(ncell(sr.30), prob=0.3, size=1)#random values from a bernoulli distribution
plot(sr.30)

#simple random with 30% habitat
sr.10 <- raster(ncol=dimX, nrow=dimY, xmn=0, xmx=dimX, ymn=0, ymx=dimY)
sr.10[] <- rbinom(ncell(sr.30), prob=0.1, size=1)#random values from a bernoulli distribution
plot(sr.10)

#------------------------------------#
#Fractal landscapes
#------------------------------------#

voss <- voss2d(g=7, H=0.7)#g sets dimensions; g=4: 16x16; g=7: 128 x 128
str(voss)

voss1.thres <- quantile(voss$z, prob=0.1)
voss3.thres <- quantile(voss$z, prob=0.3)

voss$z1 <- ifelse(voss$z<voss1.thres, 1, 0)
voss$z3 <- ifelse(voss$z<voss3.thres, 1, 0)

image(voss$x, voss$y, voss$z1, xlab="x", ylab="y",main="parameter H=0.7")#white is habitat
image(voss$x, voss$y, voss$z3, xlab="x", ylab="y",main="parameter H=0.7")#white is habitat

#plot with raster
voss.raster <- raster(as.matrix(voss$z))
plot(voss.raster,axes=F, box=F, legend=F)

voss3.raster <- raster(as.matrix(voss$z3))
plot(voss3.raster,axes=F, box=F, legend=F)

voss1.raster <- raster(as.matrix(voss$z1))
plot(voss1.raster,axes=F, box=F, legend=F)

#------------------------------------#
#Modified random clusters
#------------------------------------#

tempmask <- make.mask(nx = dimX, ny = dimX, spacing = 1)

p55A3 <- randomHabitat(tempmask, p = 0.55, A = 0.3)
p55A1 <- randomHabitat(tempmask, p = 0.55, A = 0.1)

str(p55A3)
plot(p55A3, dots = FALSE, col = "green")
plot(p55A1, dots = FALSE, col = "green")
