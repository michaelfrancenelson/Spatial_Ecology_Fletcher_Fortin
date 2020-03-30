########################################################
########################################################
#Fletcher and Fortin 2019
#Chapter 4: Spatial dispersion and point data
########################################################
########################################################

#load packages
library(spatstat)          #for point pattern analyses; version 1.56-1 used
library(raster)            #for raster covariate data; version 2.6-7 used

#set working directory where data were downloaded
setwd(choose.dir())

###############################################
#4.3.3 creating point pattern data
###############################################

#import the data from directory
cactus <- read.csv("cactus.csv")
boundary <- read.csv("cactus_boundaries.csv",header=T)

#create spatstat objects
ppp.window <- owin(xrange=c(boundary$Xmin, boundary$Xmax),
                 yrange=c(boundary$Ymin, boundary$Ymax))
ppp.cactus <- ppp(cactus$East, cactus$North, window=ppp.window)

#plot raw data
plot(ppp.cactus)

#summary information
summary(ppp.cactus)

#density plots
plot(density(ppp.cactus))
plot(density(ppp.cactus,1))  #alter smoothing parameter

#contour plot
contour(density(ppp.cactus, 1))

#quadrat counts
Q <- quadratcount(ppp.cactus, nx = 4, ny = 4)#counts in 12.5x12.5m quadrats

#plot
plot(ppp.cactus, cex = 2)
plot(Q, add = TRUE, cex = 1)

#chi-sq test for complete spatial randomness, CSR
quadrat.test(ppp.cactus, nx = 4, ny = 4, method="Chisq")

##############################################
#4.3.4 Univariate point patterns
##############################################

#-----------------------#
#Ripley's K function:
#-----------------------#

Knone <- Kest(ppp.cactus, correction="none")

#plot K
plot(Knone, legend=F)

#plot L with 1:1 expectation
Lnone <- Lest(ppp.cactus, correction="none")
plot(Lnone, legend=F)

#plot L with 0 expectation
plot(Lnone, . - r~r, legend=F)

#isotropic edge correction
Liso <- Lest(ppp.cactus, correction="isotropic")
plot(Liso, . - r~r, legend=F)

#translate (toroidal) edge correction
Ltrans <- Lest(ppp.cactus, correction="trans")
plot(Ltrans, . - r~r, legend=F)

#Monte Carlo simulations to calculate a global and pointwise confidence envelope under CSR
Lcsr <- envelope(ppp.cactus, Lest, nsim=99, rank=1, correction="trans", global=F)
Lcsr.g <- envelope(ppp.cactus, Lest, nsim=99, rank=1, correction="trans", global=T)

#plot point-wise envelope
plot(Lcsr, . - r~r, shade=c("hi", "lo"), legend=F)

#plot global envelope
plot(Lcsr.g, . - r~r, shade=c("hi", "lo"), legend=F)

#----------------------------------------#
#Pair correlation function, g
#----------------------------------------#

Ptrans <- pcf(ppp.cactus, correction="trans")

#plot
plot(Ptrans, legend=FALSE, xlim=c(0.5,14))

#Monte Carlo simulations for confidence envelope
Penv <- envelope(ppp.cactus,pcf, nsim=99, rank=1, stoyan=0.15, correction="trans", global=F)#stoyan = bandwidth; set to default

#plot pcf with envelope
plot(Penv, shade=c("hi", "lo"), legend=FALSE, xlim=c(0.5,14))

#alter bandwidth on pcf
Penv.coarse <- envelope(ppp.cactus, pcf, nsim=99, rank=1, stoyan=0.3, correction="trans", global=F)

#plot
plot(Penv.coarse, shade=c("hi", "lo"), legend=F, xlim=c(0.5,14))

#-----------------------#
#G function
#-----------------------#

Gtrans <- Gest(ppp.cactus, correction="rs")

#plot
plot(Gtrans, legend=F)

#Monte Carlo simulations for confidence envelope
Genv <- envelope(ppp.cactus, Gest, nsim=99, rank=1, correction="rs", global=F)

#plot G with envelope
plot(Genv, shade=c("hi", "lo"), legend=F)

#nearest neighbor dist
nn.dist <- nndist(ppp.cactus)
max(nn.dist)

#add cdf of nearest neighbor dist to plot
plot(ecdf(nn.dist), add=T)

##############################################
#4.3.5 Marked point patterns
##############################################

#For  call 'Z', which has the data on bugs
cactus$CheliPA <- as.factor(ifelse(cactus$chelinidea>0, "presence", "absence"))

#makes a new object with marks, amenable to analysis in spatstat
ppp.PA <- ppp(cactus$East, cactus$North, window=ppp.window, marks=cactus$CheliPA)

#inspect
summary(ppp.PA)

#plot
plot(ppp.PA)
plot(split(ppp.PA))

#Point pattern only considering Chelinidea
cheli.data <- subset(cactus, chelinidea>0)
ppp.bug <- ppp(cheli.data$East, cheli.data$North, window=ppp.window)

Lbug <- envelope(ppp.bug, Lest, nsim=99, rank=1, i="presence", global=F)

#plot with envelope
plot(Lbug, . - r~r, shade=c("hi", "lo"), legend=F)

#Bivariate PPA based on a random-labeling:
Lmulti <- envelope(ppp.PA, Lcross, nsim=99, rank=1, i="presence", global=F, simulate=expression(rlabel(ppp.PA)))

#plot with envelope
plot(Lmulti, . - r~r, shade=c("hi", "lo"), legend=F)

#-------------------------------#
#Continuous marks
#-------------------------------#

#makes a new object with marks, amenable to analysis in spatstat
ppp.area <- ppp(cactus$East, cactus$North, window=ppp.window, marks=cactus$Area)

#inspect
summary(ppp.area)

#plot
plot(ppp.area)

#mark correlation function
mcf.area <- markcorr(ppp.area)

#plot
plot(mcf.area, legend=F)

#Monte Carlo simulations for confidence envelope
MCFenv <- envelope(ppp.area, markcorr, nsim=99, correction="iso", global=F)

#plot with envelope
plot(MCFenv,  shade=c("hi", "lo"), legend=F)

#######################################################
#4.3.6 Inhomogenous point processes
#######################################################

#---------------------------------#
#point process models
#---------------------------------#

#point process model with trend
pp.xy <- ppm(ppp.cactus, ~ x + y)
AIC(pp.xy)
summary(pp.xy) #Warning: singular matrix; need to re-scale x-y coordinates

#Rescale coordinates via centering (also see rescale function)
centerx <- (boundary$Xmax + boundary$Xmin)/2
centery <- (boundary$Ymax + boundary$Ymin)/2

ppp.windows <- owin(xrange=c(boundary$Xmin - centerx, boundary$Xmax - centerx),
                   yrange=c(boundary$Ymin- centery, boundary$Ymax- centery))
ppp.cactuss <- ppp(cactus$East- centerx, cactus$North- centery, window=ppp.windows)

#re-fit with rescaled coordinates and window
pp.xy <- ppm(ppp.cactuss, ~ x + y)
summary(pp.xy)
AIC(pp.xy)
AIC(pp.xy2)

#no trend(homogeneous point process)
pp.int <- ppm(ppp.cactuss, ~1)
summary(pp.int)
AIC(pp.int)

#point process model with quadratic trend
pp.xy2 <- ppm(ppp.cactuss, ~ polynom(x, y, 2))
summary(pp.xy2)
AIC(pp.xy2)

#point process model based on vegetation covariate
veg.height <- read.csv('cactus_matrix.csv', header=T)

#inspect
head(veg.height)

#make a square matrix for creating im object
veg.height <- veg.height[order(veg.height$x, veg.height$y), ]#sort
veg.height.mat <- matrix(NA, nrow=length(unique(veg.height$x)), ncol=length(unique(veg.height$y)))
veg.height.mat[] <- veg.height$Height

#create im object
cov.veg <- im(mat=veg.height.mat,
              xrange=c(boundary$Xmin - centerx, boundary$Xmax - centerx),
              yrange=c(boundary$Ymin- centery, boundary$Ymax- centery))

#fit ppm model with vegetation
pp.veg <- ppm(ppp.cactuss, ~veg, covariates=list(veg=cov.veg))
AIC(pp.veg)
summary(pp.veg)

#plot relationship
plot(effectfun(pp.veg, "veg", se.fit=T))

#data frame for comparing AIC
data.frame(model = c("int", "xy", "xy^2", "veg"),
           AIC = c(AIC(pp.int), AIC(pp.xy), AIC(pp.xy2), AIC(pp.veg)))

#plots
plot(cov.veg)
plot(ppp.cactuss, pch=21, bg="white", add=T)
persp(cov.veg, theta=10)
plot(predict(pp.xy2, type = "trend"))
plot(ppp.cactus, add=T)

#Inhomogeneous L function that accounts for this trend
pp.xy.pred <- predict.ppm(pp.xy2, type="trend")
Lxycsr <- envelope(ppp.cactus, Linhom, nsim=99, rank=1, correction="translate", simulate=expression(rpoispp(pp.xy.pred)), global=FALSE)

#plot
plot(Lxycsr, . - r~r, shade=c("hi", "lo"), legend=FALSE)

#######################################
#4.3.7 Alternative null models
#######################################

Kthomas <- kppm(ppp.cactus, ~1, "Thomas", method='palm')#method 'palm' approximates likelihood based on 'Fry plots'
Kmatern <- kppm(ppp.cactus, ~1, "MatClust", method='palm')

#Pair correlation function for Matern process
pcfmatern <- kppm(ppp.cactus, ~1, "MatClust", statistic = "pcf")

#Compare models with AIC
AIC(Kthomas)
AIC(Kmatern)

#Monte Carlo envelope for Thomas process
Kthomas.env <- envelope(Kthomas, Lest, nsim=99, rank=1, global=F)

#plot
plot(Kthomas.env, . - r~r, shade=c("hi", "lo"), legend=F)

###########################################
#4.3.8 Simulating point patterns
###########################################

#get intensity, lambda, from our point pattern
intensity(ppp.cactus)

#Simulate a homogeneous point pattern 4 times
sim.pp <- rpoispp(lambda=intensity(ppp.cactus), nsim=4, win=ppp.window)

#plot
plot(sim.pp)

#access the x-y coords for simulation 1
sim.pp[[1]]$x
sim.pp[[1]]$y

#inhomogeneous point pattern
summary(pp.xy2)#summary of best ppm model

#function based on ppm coefficients
pp2.fun<-function(x, y) {exp(pp.xy2$coef[1]+pp.xy2$coef[2]*x+pp.xy2$coef[3]*y+pp.xy2$coef[4]*I(x^2)+
  pp.xy2$coef[5]*x*y+pp.xy2$coef[6]*I(y^2))}

#simulate using function
pp2.sim <- rpoispp(pp2.fun, nsim=2, win=ppp.windows)

#Simulate from a pixel image of expected value from the ppm
pp2.sim.exp <- rpoispp(pp.xy.pred, nsim=2)

#plot
plot(pp2.sim)
plot(pp2.sim.exp)


