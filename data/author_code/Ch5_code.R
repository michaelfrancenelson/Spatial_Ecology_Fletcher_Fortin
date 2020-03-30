########################################################
########################################################
#Fletcher and Fortin 2019
#Chapter 5: spatial dependence
########################################################
########################################################

#load packages
library(pgirmess)         #for simple correlograms; version 1.6.9 used
library(ncf)              #for spline correlograms; version 1.2-5 used
library(spdep)            #for general correlograms; version 0.7-8 used
library(geoR)             #for variograms/kriging; version 1.7-5.2.1 used
library(gstat)            #for variograms/kriging; version 1.1-6 used
library(RandomFields)     #for simulating spatial random fields/spatial dependence; version 3.1.50 used
library(raster)           #for raster covariate data; version 2.6-7 used
library(waveslim)         #for multi-scale dependence; version 1.7.5 used
library(fields)           #for multi-scale dependence; version 9.6 used
library(reshape2)         #for re-formatting data; version 1.4.3 used
library(vegan)            #for multi-scale dependence; version 2.5-2 used
library(adespatial)       #for multi-scale dependence; version 0.3-0 used

#set working directory where data were downloaded
setwd(choose.dir())

###############################################
#5.3.3 Correlograms
###############################################

#load data
matrix <- read.csv('cactus_matrix.csv', header=T)

#inspect
head(matrix)

#plot
plot(matrix[,"y"] ~ matrix[,"x"],
     pch=21, cex=1.2,
     bg=gray.colors(12)[cut(matrix[,"Height"], breaks = 12)])

#plot distribution of vegetation height
hist(matrix[,"Height"], xlab="Vegetation height (cm)",
     main="histogram of vegetation height")

#calculate distance matrix
coords <- cbind(matrix$x, matrix$y)
colnames(coords) <- c("x", "y")
distmat <- as.matrix(dist(coords))

#inspect
dim(distmat)
max(distmat)

#Maximum distance to consider in correlogram/variogram (~1/2 to 2/3 total dist)
maxdist <- 2/3*max(distmat)
maxdist

#--------------------------------------#
#Correlogram with pgirmess
#--------------------------------------#

correlog.pgirmess <- pgirmess::correlog(coords, matrix$Height, method="Moran",
                            nbclass=14, alternative = "two.sided")

#Moran and P values for each distance class
round(correlog.pgirmess,2)

#plot
plot(correlog.pgirmess[,1], correlog.pgirmess[,2],
     xlab="Distance (m)", ylab="Moran's I")
abline(h=0)

#--------------------------------------#
#Correlograms with ncf
#--------------------------------------#

#Correlogram with non-parameteric test of significance
correlog.ncf <- ncf::correlog(x = matrix$x, y = matrix$y, z = matrix$Height,
                         increment=5, resamp=99)

#plot
plot(correlog.ncf)
abline(h=0)

#Spline correlogram with 95% pointwise bootstrap confidence intervals
spline.corr <- spline.correlog(x = matrix$x, y = matrix$y, z = matrix$Height,
                               xmax = maxdist, resamp=99, type="boot")

#plot
plot(spline.corr)

#--------------------------------------#
#Moran's I test with spdep
#--------------------------------------#

#make a neighborhood list
neigh <- dnearneigh(x=coords, d1=0, d2=3, longlat=F)#d1 is minimum distance, d2 is max distance

#plot the neighorhood
plot(neigh,coordinates(coords))

#create weights for the neighbors
wts <- nb2listw(neighbours=neigh, style='W', zero.policy=T)#W = row-standardized weights

#Moran's I test with normal approximation versus Monte Carlo permutation test
mor.mc <- moran.mc(x=matrix$Height, listw=wts, nsim=999, zero.policy=T) #Monte Carlo
mor.norm <- moran.test(x=matrix$Height, listw=wts, randomisation=F, zero.policy=T)#normal approximation

#inspect
mor.mc
mor.norm

#--------------------------------------#
#Correlograms with spdep
#--------------------------------------#

#repeat above with a for loop using consecutive lag distances

#Create data frame for storing output
correlog.sp <- data.frame(dist=seq(5, maxdist, by=5),
                        Morans.i=NA, Null.lcl=NA, Null.ucl=NA, Pvalue=NA)
#inspect
head(correlog.sp)

#then do a for loop to calculate Moran's I for lag distances
for (i in 1:nrow(correlog.sp)){

  d.start <- correlog.sp[i,"dist"]-5
  d.end <- correlog.sp[i,"dist"]

  neigh <- dnearneigh(x=coords, d1=d.start, d.end, longlat=F)
  wts <- nb2listw(neighbours=neigh, style='W', zero.policy=T)
  mor.i <- moran.mc(x=matrix$Height, listw=wts, nsim=99, alternative="greater", zero.policy=T)

  #summarize results from spdep
  correlog.sp[i, "dist"] <- (d.end+d.start)/2                                    #mean dist
  correlog.sp[i, "Morans.i"] <- mor.i$statistic 								                 #observed Moran's I
  correlog.sp[i, "Null.lcl"] <- quantile(mor.i$res, probs = 0.025,na.rm = TRUE)  #lower null envelope
  correlog.sp[i, "Null.ucl"] <- quantile(mor.i$res, probs = 0.975,na.rm = TRUE)  #upper null envelope
  correlog.sp[i, "Pvalue"] <- mor.i$p.value									                     #p-value for Moran's I at that distance category
}

#plot
plot(y=correlog.sp$Morans.i, x=correlog.sp$dist,
     xlab="Lag Distance(m)", ylab="Moran's I", ylim=c(-0.3,0.3))         #ylim provides limit on y-axis between -1 and 1
abline(h=0)                                                              #0 reference
lines(correlog.sp$dist, correlog.sp$Null.lcl,col = "red")	               #add the null lcl to the plot
lines(correlog.sp$dist, correlog.sp$Null.ucl,col = "red")	               #add the null ucl to the plot

#############################################
#5.3.4 Variograms
#############################################

#----------------------------------#
#in geoR
#----------------------------------#

#create geoR object
geoR.veg <- as.geodata(matrix)

#plot
plot(geoR.veg)

#Empirical semivariogram
emp <- variog(geoR.veg, max.dist=maxdist)

#plot
plot(emp)

#standardize breaks
emp <- variog(geoR.veg, max.dist=maxdist, breaks=c(seq(0,maxdist,by=3)))

#plot variogram
plot(emp)

#----------------------------------#
#in gstat
#----------------------------------#

gstat.veg <- matrix
coordinates(gstat.veg) = ~x + y

#Empirical semivariogram
emp.gstat <- variogram(Height ~ 1, cutoff=maxdist, width=3, gstat.veg)

#plot variogram
plot(emp.gstat)

#----------------------------------#
#anisotropy check
#----------------------------------#

#Directional variogram in geoR
emp4 <- variog4(geoR.veg, max.dist=maxdist)

#plot directional variogram
plot(emp4)
plot(emp4, legend=F)

#Directional variogram in gstat
emp4.gstat <- variogram(Height ~ 1, cutoff=maxdist, alpha=c(0,45,90,135), gstat.veg)

#plot directional variogram
plot(emp4.gstat)

#-------------------------------------------------------#
#Model-based variograms with likelihood fitting in geoR
#-------------------------------------------------------#

#Exponential variogram
mlexp <- likfit(geoR.veg, cov.model="exp", ini=c(700,10))

#Spherical variogram
mlsph <- likfit(geoR.veg, cov.model="sph", ini=c(700,10))

#inspect
summary(mlexp)
summary(mlsph)
AIC(mlexp,mlsph)

#plot
plot(emp)
lines(mlexp, col="blue") #note this appears off on my plot panel, but extracting values and plotting in Fig. 5.8 shows better fit
lines(mlsph, col="red")

#Monte Carlo envelopes
emp.env <- variog.mc.env(geoR.veg, obj.var=emp)

#plot
plot(emp, envelope=emp.env)
lines(mlsph, col="red")

#--------------------------------------------------#
#Model-based variograms with least squares in gstat
#--------------------------------------------------#

#inspect models that gstat can fit
vgm()#fits more models than geoR
show.vgms()#plots examples of the various variograms

#Spherical variogram
sph.gstat <- fit.variogram(emp.gstat, vgm(500, "Sph", 15,1)) #in vgm(psill, model, range, nugget)

#Exponential variogram
exp.gstat <- fit.variogram(emp.gstat, vgm("Exp"))

#inspect
exp.gstat
sph.gstat

#plot
plot(emp.gstat, exp.gstat)
plot(emp.gstat, sph.gstat)

#################################################
#5.3.5 Kriging and interpolation
#################################################

#Create grid with intervals of 1 unit (1-m)
new.grid.1m <- expand.grid(0:max(matrix$x), 0:max(matrix$y))

#Maximum likelihood estimates from the variogram modeling
mlexp$nugget
mlexp$cov.pars[1]
mlexp$cov.pars[2]

#Ordinary kriging
krig.geoR.exp <- krige.conv(geoR.veg,locations=new.grid.1m,
                          krige=krige.control(cov.pars=c(mlexp$cov.pars[1], mlexp$cov.pars[2]), nugget = mlexp$nugget,
                                              cov.model="exp", type.krige="OK"))

#inspect
summary(krig.geoR.exp)
hist(krig.geoR.exp$predict)
hist(matrix$Height)
krig.geoR.exp$krige.var

#plot
image(krig.geoR.exp, main="krigged estimates")
image(krig.geoR.exp, val=sqrt(krig.geoR.exp$krige.var), main="kriging SE")

#----------------------#
#kriging with gstat
#----------------------#
new.grid.1m.gstat <- expand.grid(x=0:max(matrix$x), y=0:max(matrix$y))#need labels for coords

#convert to sp object
gridded(new.grid.1m.gstat) = ~x + y

krig.gstat <- krige(Height ~ 1, gstat.veg, new.grid.1m.gstat, model = exp.gstat)

#Plot
image(krig.gstat, main="krigged estimates-gstat")
image(krig.geoR.exp, main="krigged estimates-geoR")

cor(krig.geoR.exp$predict, krig.gstat$var1.pred)

#Inverse distance weighting in gstat
idw.gstat <- idw(Height ~ 1, gstat.veg, new.grid.1m.gstat)#idp-the power used; default power=2

#Plot
image(idw.gstat, main="IDW-gstat")

#Correlation between idw, kriging in gstat, kriging in geoR
round(cor(cbind(geoR.exp=krig.geoR.exp$predict,
                gstat.exp=krig.gstat$var1.pred,
                gstat.idw=idw.gstat$var1.pred)), 3)

#################################################
#5.3.5 Simulating spatially autocorrelated data
#################################################

#dimension of grid
dimx=0:50
dimy=0:50

#variogram models to simulate
model.exp <- RMexp(var=mlexp$cov.pars[1], scale=mlexp$cov.pars[2])+
  RMnugget(var=mlexp$nugget) + RMtrend(mean=mean(matrix$Height))

#simulate
sim.exp <- RFsimulate(model = model.exp, x=dimx, y=dimy)#creates spatial grid
sim.exp.mat <- as.matrix(sim.exp)

#plot with image
image(dimx, dimy, sim.exp.mat,
      xlab="x-coordinate", ylab="y-coordinate",
      main="Random Map (Exponential Variogram)")#yellow is higher value

#plot with raster
RMexp.grid <- raster(sim.exp)
plot(RMexp.grid)

###############################################
#5.3.6.1 Wavelets: Multiscale decomposition
###############################################

#format data
matrix.mat <- acast(matrix, x ~ y, value.var="Height")
dim(matrix.mat)
max.scale <- 4

#DWT: Discrete Wavelet Transform
x.dwt <- dwt.2d(matrix.mat[1:16, 1:16], 'haar', J=max.scale)

#inspect
str(x.dwt)

#plots
plot(x.dwt, plot=F)
plot(x.dwt)

#------------------------------------------#
#Create scalogram:
#------------------------------------------#

#Sum the wavelet spectrums
Total.var <-  (sum(x.dwt$LH1^2 + x.dwt$HL1^2 + x.dwt$HH1^2)
               + sum(x.dwt$LH2^2 + x.dwt$HL2^2 + x.dwt$HH2^2)
               + sum(x.dwt$LH3^2 + x.dwt$HL3^2 + x.dwt$HH3^2)
               + sum(x.dwt$LH4^2 + x.dwt$HL4^2 + x.dwt$HH4^2))


#proportional variance
x.lev.1 <- (sum(x.dwt$LH1^2 + x.dwt$HL1^2 + x.dwt$HH1^2)) / Total.var
x.lev.2 <- (sum(x.dwt$LH2^2 + x.dwt$HL2^2 + x.dwt$HH2^2)) / Total.var
x.lev.3 <- (sum(x.dwt$LH3^2 + x.dwt$HL3^2 + x.dwt$HH3^2)) / Total.var
x.lev.4 <- (sum(x.dwt$LH4^2 + x.dwt$HL4^2 + x.dwt$HH4^2)) / Total.var

var.all.dwt <- c(x.lev.1, x.lev.2, x.lev.3, x.lev.4)
sum(var.all.dwt)

#Scalogram: plotting global Wavelet spectrum profiles
plot (var.all.dwt, pch=21, type="b", lwd=1, ylab="Average Variance", xlab="Scale")

#Map Wavelet values according to scales
par(pty="s")#set aspect to square for plotting square grid
image.plot((x.dwt$LH1^2 + x.dwt$HL1^2 + x.dwt$HH1^2), axes=F)
image.plot((x.dwt$LH2^2 + x.dwt$HL2^2 + x.dwt$HH2^2), axes=F)
image.plot((x.dwt$LH3^2 + x.dwt$HL3^2 + x.dwt$HH3^2), axes=F)

############################################
#5.3.6.2 Eigenvector decomposition
############################################

#PCNM on distance matrix
xypcnm <- pcnm(dist(coords))

#threshold distance for truncation
xypcnm$threshold

#inspect
str(xypcnm)

#Eigenvector
head(xypcnm$vectors,2)

#Map PCNM at different scales
#pcnm1
plot(matrix[,"y"] ~ matrix[,"x"],
     pch=15, cex=1.5,
     col=gray.colors(12)[cut(xypcnm$vectors[,1],breaks = 12)])
#pcnm13
plot(matrix[,"y"] ~ matrix[,"x"],
     pch=15, cex=1.5,
     col=gray.colors(12)[cut(xypcnm$vectors[,13],breaks = 12)])

#plot pcnm1 with raster
pcnm1.raster <- rasterFromXYZ(data.frame(x=matrix$x, y=matrix$y, z=xypcnm$vectors[,1]))
plot(pcnm1.raster)

#format data for forward selection
height <- matrix$Height
xypcnm.df <- data.frame(xypcnm$vectors)

#fit global model of eigenvectors to vegetation and extract adjusted R^2
xypcnm.full <- lm(height ~ ., data=xypcnm.df)
R2adj <- summary(xypcnm.full)$adj.r.squared

#Determine which PCNMs contribute to explain the spatial pattern of the data via forward selection
xypcnm.for <- forward.sel(height, xypcnm$vectors, adjR2thresh = R2adj,
                        alpha=0.005, Xscale=F, nperm = 999)

xypcnm.for
