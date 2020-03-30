########################################################
########################################################
#Fletcher and Fortin 2019
#Chapter 2: Scale
########################################################
########################################################

#load packages
library(raster)      #for raster data; version 2.6-7 used
library(rgdal)       #for raster data, projections; version 1.3-4 used
library(rgeos)       #for buffer analysis; version 0.3-28 used
library(fields)      #for kernel analysis; version 9.6 used
library(Matrix)      #for kernel analysis, version 1.2-14 used

#set working directory where you downloaded the data
setwd(choose.dir())

#############################
#2.3.3 a simple example
#############################

set.seed(16)
toy <- raster(ncol=6, nrow=6, xmn=1, xmx=6, ymn=1, ymx=6)
toy[] <- rpois(ncell(toy), lambda=3)

#plot
plot(toy, axes=F, box=F)
text(toy, digits=2)

#check cell labeling/order
ncell(toy)
toy2 <- toy
toy2[] <- 1:ncell(toy)

#plot
plot(toy2)
text(toy2, digits=2)

#increase the grain
toy_mean <- aggregate(toy, fact=2, fun=mean)#mean value
toy_maj <- aggregate(toy, fact=2, fun=modal)#majority rule

#plot mean rule
plot(toy_mean)
text(toy_mean,digits=1)

#plot majority rule
plot(toy_maj)
text(toy_maj)

#contrast means/variances
cellStats(toy, mean) #or: mean(toy[])
cellStats(toy, var)

cellStats(toy_mean, mean)
cellStats(toy_mean, var)

cellStats(toy_maj, mean)
cellStats(toy_maj, var)

#decrease the grain
toy_dis2 <- disaggregate(toy, fact=2)
toy_dis2_bilinear <- disaggregate(toy, fact=2, method='bilinear')

#plot
plot(toy_dis2, axes=F, box=F)
plot(rasterToPolygons(toy_dis2), add=TRUE, border='gray50', lwd=1)
text(toy_dis2, cex=0.9)

#plot
plot(toy_dis2_bilinear, axes=F, box=F)
plot(rasterToPolygons(toy_dis2_bilinear), add=TRUE, border='gray50', lwd=1)
text(toy_dis2_bilinear, digits=1, cex=0.6)

#decrease the extent
e <- extent(2, 4, 2, 4)#first create new, smaller extent
toy_crop <- crop(toy, e)

#plot
plot(toy)
plot(toy_crop)

#increase the extent
e <- extent(0, 7, 0, 7)#first create new, bigger extent
toy_big <- extend(toy,e)

#plot
plot(toy)
plot(toy_big)

####################################
#2.3.4.1 multi-scale analysis
####################################

#------------------#
#nlcd
#------------------#

nlcd<-raster("nlcd2011SE")

#inspect
proj4string(nlcd)   #from sp
projection(nlcd)    #alternative function from raster package
crs(nlcd)           #alternative function from raster package (replaces 'projection')

#set projection
nlcd_proj <- projection(nlcd)

#inspect raster properties
res(nlcd)
ncell(nlcd)
extent(nlcd)

#check raster values
levels(nlcd)
nlcd <- as.factor(nlcd) #convert to factors
levels(nlcd)

#-------------------------------#
#site locations: shp file
#-------------------------------#

#site and reptile data
sites <- readOGR("reptiledata")

#inspect
class(sites)
proj4string(sites)
proj4string(sites) <- nlcd_proj #set projection
summary(sites)
head(sites, 2)

#plot with custom color scheme
my_col <- c("black","blue","darkorange","red","darkred","grey30","grey50", "lightgreen",
            "green", "darkgreen", "yellow", "goldenrod", "purple", "orchid","lightblue", "lightcyan")

#plot
plot(nlcd, col=my_col, axes=F, box=F)
plot(sites, add=T)

#subset points to remove corn land use
sites <- subset(sites, management!="Corn")
nrow(sites)

#crop raster to 10 km from sampling points: determine min/max coordinates for new extent
x.min <- min(sites$coords_x1) - 10000
x.max <- max(sites$coords_x1) + 10000
y.min <- min(sites$coords_x2) - 10000
y.max <- max(sites$coords_x2) + 10000

extent.new <- extent(x.min, x.max, y.min, y.max)
nlcd <- crop(nlcd, extent.new)

#create a binary forest layer using nlcd as template
forest <- nlcd
values(forest) <- 0 #set to zero

#reclassify:
#with raster algebra; this is slow
forest[nlcd==41 | nlcd==42 | nlcd==43] <- 1  #locations with evergreen + mixed forest + deciduous forest

#reclassify with reclassify function is faster
reclass <- c(rep(0,7), rep(1,3), rep(0,6))
nlcd.levels <- levels(nlcd)[[1]]

#create reclassify matrix: first col: orginal; second: change to
reclass.mat <- cbind(levels(nlcd)[[1]], reclass)
reclass.mat

#reclassify
forest <- reclassify(nlcd, reclass.mat)

#plot
plot(forest)
plot(sites, pch=21, col="white", add=T)

buf1km <- 1000
buf5km <- 5000

#buffer first site
buffer.site1.1km <- buffer(sites[1,], width=buf1km)
plot(buffer.site1.1km)

#buffer using rgeos, which is more flexible
buffer.site1.1km <- gBuffer(sites[1,], width=buf1km, quadsegs=10)
buffer.site1.5km <- gBuffer(sites[1,], width=buf5km, quadsegs=10)

#zoom in on plot for 5 km buffer at site 1
#can provide object to zoom on or click twice on layer

#plot
zoom(nlcd, buffer.site1.5km, col=my_col, box=F)
plot(buffer.site1.1km, border="red", lwd = 3, add=T)
plot(buffer.site1.5km, border="red", lwd = 3, add=T)
points(sites[1,], pch=19, cex=2)
plot(sites[1,], col="grey20", bg="black", pch=22, cex=1, add=T)

#view just forest within buffer
zoom(forest, buffer.site1.1km, box=F)
plot(buffer.site1.1km, border="red", lwd = 3,add=T)
dev.off() #end zooming

#calculate forest area within buffer
buffer.forest1.1km <- crop(forest, buffer.site1.1km)
buffer.forest1.1km <- mask(buffer.forest1.1km, buffer.site1.1km)

#plot forest within buffer
plot(buffer.forest1.1km)

#calculate percent forest cover
grainarea <- res(forest)[[1]]^2/10000#in ha
bufferarea <- (3.14159*buf1km^2)/10000#pi*r^2
forestcover1km <- cellStats(buffer.forest1.1km, 'sum')*grainarea
percentforest1km <- forestcover1km/bufferarea*100
percentforest1km

#-----------------------------------------#
#Function that puts all the steps together
#requires:
#  points: one set of x,y coordinates
#  size: the buffer size (radius), in m
#  landcover: a binary land-cover map
#  grain: the resolution of the map
#-----------------------------------------#

BufferCover <- function(coords, size, landcover, grain){

  bufferarea.i <- pi*size^2/10000                             #ha; size must be in m
  coords.i <- SpatialPoints(cbind(coords[i, 1],coords[i, 2])) #create spatial points from coordinates
  buffer.i <- gBuffer(coords.i, width=size)                   #buffer from rgeos
  crop.i <- crop(landcover, buffer.i)                         #crop with raster function
  crop.NA <- setValues(crop.i, NA)                            #empty raster for the rasterization
  buffer.r <- rasterize(buffer.i, crop.NA)                    #rasterize buffer
  land.buffer <- mask(x=crop.i, mask=buffer.r)                #mask by putting NA outside the boundary
  coveramount<-cellStats(land.buffer, 'sum')*grain            #calculate area
  percentcover<-100*(coveramount/bufferarea.i)                #convert to %

  return(percentcover)
}

#create empty vector for storing output
f1km <- rep(NA, length = nrow(sites))
f2km <- rep(NA, length = nrow(sites))

#with for loop (all five buffers: 910s; <=3km: 228s)
for(i in 1:nrow(sites)) {
  f1km[i] <- BufferCover(coords=sites,size=1000,landcover=forest,grain=grainarea)
  f2km[i] <- BufferCover(coords=sites,size=2000,landcover=forest,grain=grainarea)
  print(i)
}

#make a data frame
forest.scale <- data.frame(site=sites$site,
                         x=sites$coords_x1, y=sites$coords_x2,
                         f1km=f1km, f2km=f2km)

#plot
plot(f1km, f2km)

#aggregate forest landcover to grain reflected in sampling
forest200 <- aggregate(forest, fact=7, fun=modal)

#inspect
res(forest200) #210x210 res

####################################
#2.3.4.2 Scale of effect
####################################

#----------------------------------------#
#2.3.4.2 Buffer analysis
#----------------------------------------#

#herp data
flsk <- read.csv("reptiles_flsk.csv", header=T)
flsk <- merge(flsk, forest.scale, by="site", all=F)

#glms at 2 scales; see text for more scales considered
pres.1km <- glm(pres ~ f1km, family = "binomial", data = flsk)
pres.2km <- glm(pres ~ f2km, family = "binomial", data = flsk)

#summary information
summary(pres.1km)
summary(pres.2km)

#likelihoods
logLik(pres.1km)
logLik(pres.2km)

#accessing coefficients
pres.1km.ci <- confint(pres.1km)
pres.2km.ci <- confint(pres.2km)
pres.2km.ci

#----------------------------------------#
#2.3.4.2 Kernel analysis
#----------------------------------------#

#recreate simple logistic regression from scratch
nll <- function(par, cov, y) {
  alpha <- par[1]
  beta  <- par[2]
  lp <- alpha + beta*cov
  p <- plogis(lp)
  loglike <- -sum(y*log(p) + (1-y)*log(1-p))#nll
  return(loglike)
}

#fit simple logistic regression model with optim
lr.buffer <- optim(par=c(0, 0), fn=nll, method='L-BFGS-B',
                  cov=flsk$f2km, y=flsk$pres, hessian=T)
lr.buffer$par
lr.buffer.vc <- solve(lr.buffer$hessian)  #var-cov matrix
lr.buffer.se <- sqrt(diag(lr.buffer.vc))  #standard errors
lr.buffer.se

#check against glm function
summary(pres.2km)$coefficients

#logistic regression with spatial kernel
nll.kernel <- function(par, D, cov, y) {
  sig <-   exp(par[1])                       #ensures sigma is non-negative
  alpha <- par[2]
  beta <-  par[3]
  cov.w <- apply(D, 1, function(x) {
    w0 <- exp(-x^2 / (2*sig^2))              #gaussian kernel
    w0[w0==1] <- 0                           #for truncated data: D > max leads to w0=1
    w <- w0/sum(w0)                          #w kernel weights; make so weights sum to 1
    sum(cov * w)                             #weighted average of the raster values
  })
  lp <- alpha + beta*cov.w                   #linear predictor
  p <-  plogis(lp)                           #back-transform to probability
  loglike <- -sum(y*log(p) + (1-y)*log(1-p)) #nll for logistic
  return(loglike)
}

#raster to data frame
forest200.df <- data.frame(rasterToPoints(forest200))

#inspect
names(forest200.df)

#dist matrix
D <- rdist(as.matrix(flsk[,c("x","y")]),
           as.matrix(forest200.df[,c("x","y")]))

#reduce size of D
D <- D/1000                                                         #in km
D[D > 10] <- 0                                                      #truncate to only consider max dist (10km)
D <- Matrix(D, sparse = TRUE)                                       #convert ot sparse matrix format
cov.subset <- which(colSums(D)!=0, arr.ind = T)                     #identify columns with all zeros->locations that are not within 10km of any site
D <- D[, cov.subset]                                                #remove columns with all zeros

#fit kernel logistic regression with optim:
lr.kernel <- optim(fn=nll.kernel, hessian=T, method='L-BFGS-B',
                   D=D, par=c(0,-6,8),                              #use starting values based on lr.buffer, adjusted for proportion rather than % cover
                   cov=forest200.df$layer[cov.subset], y=flsk$pres)

#parameters
lr.kernel$par

#model negative log-likelihoods
lr.buffer$value
lr.kernel$value

#AIC
AICkernel <- 2*lr.kernel$value + 2*length(lr.kernel$par)
AICbuffer <- 2*lr.buffer$value + 2*length(lr.buffer$par)
c(AICkernel, AICbuffer)

#to plot kernel fitted in lr.kernel
sig.fit <- exp(lr.kernel$par[1])
dist.kernel <- seq(1,10, by=0.2)
gaus.kernel <- exp(-dist.kernel^2/(2*sig.fit^2))
plot(dist.kernel, gaus.kernel)

#to compare covariates:
cov.fit.w <- apply(D, 1, function(x) {
  w0 <- exp(-x^2 / (2*sig.fit^2))          #gaussian kernel
  w0[w0==1] <- 0                           #for truncated data: D > max leads to w0=1
  w <- w0/sum(w0)                          #w kernel weights; make so weights sum to 1
  sum(forest200.df$layer[cov.subset] * w)  #weighted average of the raster values
})

#plot
plot(flsk$f2km/100, cov.fit.w)

#correlation
cor(flsk$f2km/100, cov.fit.w)



