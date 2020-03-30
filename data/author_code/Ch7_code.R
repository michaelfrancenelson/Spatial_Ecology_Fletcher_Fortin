########################################################
########################################################
#Fletcher and Fortin 2019
#Chapter 7: Species Distributions
########################################################
########################################################

#load packages
library(raster)           #for raster covariate data; version 2.6-7 used
library(reshape2)         #for re-formatting data; version 1.4.3 used
library(mgcv)             #for gams; version 1.8-24 used
library(dismo)            #for SDMs; version 1.1-4 used
library(rJava)            #for calling maxent from dismo (need Java installed); version 0.9-10 used
library(randomForest)     #for random forest SDMs; version 4.6-14 used
library(maxnet)           #maxent with maxnet; version 0.1.2 used
library(glmnet)           #needed for maxnet; version 2.0-16 used
library(MuMIn)            #for model selection; version 1.42.1 used
library(PresenceAbsence)  #for model evaluation; version 1.1.9 used
library(ecopspat)         #for model evaluation; version 3.0 used

#set working directory where data were downloaded
setwd(choose.dir())

###################################################
#7.3.2 Preparing the data
###################################################

#------------------------------------------#
#subsetting point data
#------------------------------------------#

vath.data <- read.csv(file="vath_2004.csv", header=TRUE)
vath.val <- read.csv(file="vath_VALIDATION.csv", header=TRUE)

#subset to presence-only / absence-only
vath.pres <- vath.data[vath.data$VATH==1,]
vath.abs <- vath.data[vath.data$VATH==0,]
vath.pres.xy <- as.matrix(vath.pres[,c("EASTING","NORTHING")])
vath.abs.xy <- as.matrix(vath.abs[,c("EASTING","NORTHING")])

#validation data
vath.val.pres <- as.matrix(vath.val[vath.val$VATH==1, c("EASTING","NORTHING")])
vath.val.abs <- as.matrix(vath.val[vath.val$VATH==0, c("EASTING","NORTHING")])
vath.val.xy <- as.matrix(vath.val[,c("EASTING","NORTHING")])

#------------------------------------------#
#viewing GIS data
#------------------------------------------#

#covariate maps
elev <- raster("elev.gri")                 #elevation layer
canopy <- raster("cc2.gri")                #linear gradient in canopy cover taken from PCA
mesic <- raster("mesic.gri")               #presence of mesic forest
precip <- raster("precip.gri")             #mean precip (cm)

#check maps
compareRaster(elev, canopy)
compareRaster(elev, mesic)
compareRaster(elev, precip)

#resample to align layers
mesic <- resample(x=mesic, y=elev, "ngb")            #nearest neighbor (categorical)
precip <- resample(x=precip, y=elev, "bilinear")     #for continuous data

#crop to same extent
mesic <- mask(mesic, elev)
precip <- mask(precip, elev)

#check maps
compareRaster(elev,precip, mesic)

#make 1 km wet forest
fw.1km <- focalWeight(mesic, 1000, 'circle')           #buffer in CRS units
mesic1km <- focal(mesic, w=fw.1km, fun="sum", na.rm=T)

#create raster stacck
layers <- stack(canopy, elev, mesic, mesic1km, precip)
names(layers) <- c("canopy", "elev", "mesic", "mesic1km", "precip")

#plot stack and correlations among covariates
pairs(layers, maxpixels=1000)                          #maxpixels sets upper limit on sampling raster
plot(layers)

#drop correlated layer (mesic)
layers <- dropLayer(layers, 3)

#Generate availability/background points using dismo
back.xy <- randomPoints(layers, p=vath.pres.xy, n=2000)

#inspect
head(back.xy)

#re-name columns
colnames(back.xy) <- c("EASTING","NORTHING")

#plot
plot(elev)
points(back.xy)

#extract GIS data
pres.cov <- extract(layers, vath.pres.xy)          #extracts values from layers at pres locations
back.cov <- extract(layers, back.xy)               #extracts values from layers at random locations
val.cov <- extract(layers, vath.val.xy)            #extracts values from layers at validation locations

#link data
pres.cov <- data.frame(vath.pres.xy, pres.cov, pres=1)
back.cov <- data.frame(back.xy, back.cov, pres=0)
val.cov <- data.frame(vath.val, val.cov)

#remove any potential NAs
pres.cov <- pres.cov[complete.cases(pres.cov),]
back.cov <- back.cov[complete.cases(back.cov),]
val.cov <- val.cov[complete.cases(val.cov),]

#bind presence and background points together
all.cov <- rbind(pres.cov, back.cov)

#inspect
head(all.cov)

############################################
#7.3.2.1 Envelopes
############################################

#fit model
bioclim.vath <- bioclim(layers, vath.pres.xy)

#inspect
summary(bioclim.vath)
names(layers)

#plot
plot(bioclim.vath, a=1, b=2, p=0.85)        #elev-canopy plot 85% quantile bounding box
plot(bioclim.vath, a=1, b=2, p=0.95)        #elev-canopy plot 95% quantile bounding box
plot(bioclim.vath, a=1, b=4, p=0.85)        #elev-precip plot

#mapping
bioclim.map <- predict(layers, bioclim.vath)

#plot
plot(bioclim.map, axes=F, box=F, main="bioclim")

############################################
#7.3.2.2 GLMs and GAMs
############################################

#-------------------------------#
#GLMs
#-------------------------------#

glm.vath <- glm(pres~canopy+elev+I(elev^2)+mesic1km+precip, family=binomial(link=logit), data=all.cov)

#inspect
summary(glm.vath)

#mapping
glm.map <- predict(layers, glm.vath, type="response")

#plot
plot(glm.map, axes=F, box=F, main="GLM")

#-------------------------------#
#GAMs
#-------------------------------#

#GAM (default settings with optimal knots determined by generalized cross validation)
gam.vath <- gam(pres~s(canopy)+s(elev)+s(mesic1km)+s(precip), family=binomial(link=logit), method="ML", data=all.cov)

#inspect
summary(gam.vath)

#plot relationships
plot(gam.vath, shade=T)

#Manually alter the number of knots
gam.vath.knot3 <- gam(pres~s(canopy,k=3)+s(elev,k=3)+s(mesic1km,k=3)+s(precip,k=3), family=binomial(link=logit), method="ML", data=all.cov)
gam.vath.knot6 <- gam(pres~s(canopy,k=6)+s(elev,k=6)+s(mesic1km,k=6)+s(precip,k=6), family=binomial(link=logit), method="ML", data=all.cov)

#plot relationships and compare
plot(gam.vath.knot3, shade=T)
plot(gam.vath.knot6, shade=T)

#Consider interactions among splines with tensors (this is slow; ~ 6min)
gam.vath.tensor <- gam(pres~te(canopy,elev,precip,mesic1km), family=binomial(link=logit), method="ML", data=all.cov)

#plot
plot(gam.vath.tensor, shade=T)

#Change the smoothing function
gam.vath.cr <- gam(pres~s(canopy, bs="cr")+s(elev, bs="cr")+s(mesic1km, bs="cr")+s(precip, bs="cr"), family=binomial(link=logit), method="ML", data=all.cov)

#plot
plot(gam.vath.cr, shade=T)

#evaluation of gam tuning (with evaluate function in dismo)
eval.gam <- evaluate(p=vath.val.pres, a= vath.val.abs, gam.vath, layers)
eval.gam3 <- evaluate(p=vath.val.pres, a= vath.val.abs, gam.vath.knot3, layers)
eval.gamte <- evaluate(p=vath.val.pres, a= vath.val.abs, gam.vath.tensor, layers)
eval.gamcr <- evaluate(p=vath.val.pres, a= vath.val.abs, gam.vath.cr, layers)

#inspect tuning
eval.gamcr

#evaluation with AIC
round(AIC(gam.vath, gam.vath.knot3, gam.vath.knot6, gam.vath.tensor, gam.vath.cr), 1)

#mapping
gam.map <- predict(layers, gam.vath.knot3, type="response")

#plot
plot(gam.map, axes=F, box=F, main="GAM")

############################################
#7.3.2.3 Random Forests
############################################

#random forest model (default)
rf.vath <- randomForest(as.factor(pres) ~ canopy+elev+mesic1km+precip, na.action=na.omit, data=all.cov)

#tuning model
rf.vath.tune <- tuneRF(y=as.factor(all.cov$pres), x = all.cov[,c(3:6)], stepFactor=0.5, ntreeTry=500)

#update rf model with mtry=1 based on tuning
rf.vath <- randomForest(as.factor(pres) ~ canopy+elev+mesic1km+precip, mtry=1, ntree=500, na.action=na.omit, data=all.cov)

#variable importance plot
varImpPlot(rf.vath)

#mapping
rf.map <- predict(layers, rf.vath, type="prob",index=2)

#plot
plot(rf.map, axes=F, box=F, main="RF")

############################################
#7.3.2.4 Maxent
############################################

#for Maxent to run, place the maxent.jar file in the following directory:
system.file("java",package="dismo")

#Maxent model (default)
max.vath <- maxent(layers, p=vath.pres.xy)

#Provide background points
max.vath <- maxent(layers, p=vath.pres.xy, a=back.xy)

#Tuning a maxent model
maxent.beta.3 <- maxent(layers, p=vath.pres.xy, a=back.xy,
                      args=c("betamultiplier=0.3"))
maxent.beta3 <- maxent(layers, p=vath.pres.xy, a=back.xy,
                   args=c("betamultiplier=3"))
maxent.features <- maxent(layers, p=vath.pres.xy, a=back.xy,
                      args=c("noproduct", "nohinge","nothreshold","noautofeature"))

#evaluate models
eval.max <- evaluate(p=vath.val.pres, a=vath.val.abs, max.vath, layers)
eval.max3 <- evaluate(p=vath.val.pres, a=vath.val.abs, maxent.beta3, layers)
eval.maxfeatures <- evaluate(p=vath.val.pres, a=vath.val.abs, maxent.features, layers)

#inspect
eval.max
eval.max3
eval.maxfeatures

#plot
response(max.vath, expand=0)
response(maxent.beta.3, expand=0)
response(maxent.beta3, expand=0)
response(maxent.features, expand=0)

#mapping
max.map <- predict(layers, max.vath)

#plot
plot(max.map, axes=F, box=F, main="Maxent")

#mapping with raw output (ROR)
max.raw.map <- predict(layers, max.vath, args="outputformat=raw")

#plot
plot(max.raw.map, axes=F, box=F, main="Maxent-raw")
cellStats(max.raw.map, mean)

############################################
#7.3.2.5 Point process models
############################################

#--------------------------------------------#
#GLM, GAM, and Maxent as point process models
#--------------------------------------------#

glm.ppm <- glm(pres~canopy+elev+I(elev^2)+mesic1km+precip, family=binomial(link=logit), weights = 1000^(1-pres), data=all.cov)
gam.ppm <- gam(pres~s(canopy)+s(elev)+s(mesic1km)+s(precip), family=binomial(link=logit), weights = 1000^(1-pres), method="ML", data=all.cov)
maxent.ppm <- maxent(layers, p=vath.pres.xy, a=back.xy, args=c("noremoveduplicates"))

#inspect and compare
summary(glm.vath)
summary(glm.ppm)
summary(gam.vath)
summary(gam.ppm)
summary(maxent.vath)
summary(maxent.ppm)

#--------------------------------------------#
#Maxent as point process model with maxnet
#--------------------------------------------#

maxnet.vath <- maxnet(all.cov$pres, all.cov[,c("canopy","elev","mesic1km","precip")])
maxnet.beta3.linquad <- maxnet(all.cov$pres, all.cov[,c("canopy","elev","mesic1km","precip")],
                      regmult=3,
                      maxnet.formula(all.cov$pres, all.cov[,c("canopy","elev","mesic1km","precip")], classes="lq"))

#inspect
summary(maxnet.beta3.linquad)

#plot response curves
response.plot(maxnet.vath, "canopy", type="link")      #partial plot on link scale
response.plot(maxnet.vath, "canopy", type="logistic")
response.plot(maxnet.vath, "canopy", type="cloglog")

#mapping
maxnet.cloglog.map <- predict(layers, maxnet.vath, clamp=F, type="cloglog")
maxnet.logistic.map <- predict(layers, maxnet.vath, clamp=F, type="logistic")

#correlation
pairs(stack(maxnet.cloglog.map, maxnet.logistic.map))

#plot
plot(maxnet.cloglog.map, axes=F, box=F, main="Maxnet-clog")
plot(maxnet.logistic.map, axes=F, box=F, main="Maxnet-logistic")

#################################################
#7.3.3 Interpreting environmental relationships
#################################################

#median of each variable
elev.median <- median(back.cov$elev, na.rm=T)
canopy.median <- median(back.cov$canopy, na.rm=T)
precip.median <- median(back.cov$precip, na.rm=T)
mesic1km.median <- median(back.cov$mesic1km, na.rm=T)

#range
elev.range <- seq(min(back.cov$elev, na.rm=T), max(back.cov$elev, na.rm=T), length=100)
canopy.range <- seq(min(back.cov$canopy, na.rm=T), max(back.cov$canopy, na.rm=T), length=100)

#Data frame of new data
elev.partial.data <- data.frame(expand.grid(elev=elev.range, canopy=canopy.median, precip=precip.median, mesic1km=mesic1km.median))
canopy.partial.data <- data.frame(expand.grid(elev=elev.median, canopy=canopy.range, precip=precip.median, mesic1km=mesic1km.median))

#Predict onto new data
bio.pred.elev <- predict(bioclim.vath, elev.partial.data)
bio.pred.canopy <- predict(bioclim.vath, canopy.partial.data)

glm.pred.elev <- predict(glm.vath, elev.partial.data,type="response")
glm.pred.canopy <- predict(glm.vath, canopy.partial.data,type="response")

gam.pred.elev <- predict(gam.vath, elev.partial.data,type="response")
gam.pred.canopy <- predict(gam.vath, canopy.partial.data,type="response")

rf.pred.elev <- predict(rf.vath, elev.partial.data, type="prob")
rf.pred.canopy <- predict(rf.vath, canopy.partial.data, type="prob")
rf.pred.elev <- rf.pred.elev[,2]
rf.pred.canopy <- rf.pred.canopy[,2]

max.pred.elev <- predict(max.vath, elev.partial.data)
max.pred.canopy <- predict(max.vath, canopy.partial.data)

#Data frame for plots
part.elev.df <- data.frame(elevation=elev.range,
                       bioclim=bio.pred.elev, glm=glm.pred.elev,gam=gam.pred.elev,
                       rf=rf.pred.elev,max=max.pred.elev)
part.canopy.df <- data.frame(canopy=canopy.range,
                       bioclim=bio.pred.canopy, glm=glm.pred.canopy,gam=gam.pred.canopy,
                       rf=rf.pred.canopy,max=max.pred.canopy)

#plot elevation
plot(part.elev.df$elevation, part.elev.df$bioclim, type='l', xlab="Elevation", ylab="Response", ylim=c(0,0.6))
lines(part.elev.df$elevation, part.elev.df$glm, type='l',col="red")
lines(part.elev.df$elevation, part.elev.df$gam, type='l',col="orange")
lines(part.elev.df$elevation, part.elev.df$rf, type='l',col="blue")
lines(part.elev.df$elevation, part.elev.df$max, type='l',col="purple")

#plot canopy
plot(part.canopy.df$canopy, part.canopy.df$bioclim, type='l', xlab="canopy", ylab="Response", ylim=c(0,0.7))
lines(part.canopy.df$canopy, part.canopy.df$glm, type='l',col="red")
lines(part.canopy.df$canopy, part.canopy.df$gam, type='l',col="orange")
lines(part.canopy.df$canopy, part.canopy.df$rf, type='l',col="blue")
lines(part.canopy.df$canopy, part.canopy.df$max, type='l',col="purple")

##################################################
#7.3.4 Model evaluation
##################################################

#to use PresenceAbsence Package:
#data frame format:
#column 1: siteID; column 2: validation 0/1; column 3-N: model predictions (column 3 = model 1)

#---------------------------------------#
#evaluate based on prospective sampling
#---------------------------------------#

#predictions for validation
val.cov.pred <- val.cov[,cbind("canopy", "elev", "mesic1km", "precip")]
bio.val <- predict(bioclim.vath, val.cov.pred)
glm.val <- predict(glm.vath, val.cov.pred, type="response")
gam.val <- predict(gam.vath, val.cov.pred, type="response")
rf.val <- predict(rf.vath, val.cov.pred, type="prob")
rf.val <- rf.val[,2]
max.val <- predict(max.vath, val.cov.pred)

#PresenceAbsence data frame
val.data <- data.frame(siteID=1:nrow(vath.val), obs=vath.val$VATH,
                      bio=bio.val, glm=glm.val, gam=gam.val, rf=rf.val, max=max.val)

#correlation among model predictions
round(cor(val.data[,c("bio","glm","gam","rf","max")], method="spearman"),2)

#data frame to store summary statistics
summary.eval <- data.frame(matrix(nrow=0, ncol=9))
names(summary.eval) <- c("model", "auc", "corr", "ll", "threshold", "sens", "spec", "tss", "kappa")

nmodels <- ncol(val.data)-2
detach(package:glmnet)

for(i in 1:nmodels){

  #calculate summary statistics
  auc.i <- auc(val.data, which.model=i)
  kappa.opt <- optimal.thresholds(val.data, which.model=i, opt.methods=3)
  sens.i <- sensitivity(cmx(val.data, which.model=i,threshold = kappa.opt[[2]]))
  spec.i <- specificity(cmx(val.data, which.model=i,threshold = kappa.opt[[2]]))
  tss.i<- sens.i$sensitivity +spec.i$specificity - 1
  kappa.i <- Kappa(cmx(val.data, which.model=i,threshold = kappa.opt[[2]]))
  corr.i <- cor.test(val.data[,2], val.data[,i+2])$estimate
  ll.i <- sum(log(val.data[,i+2]*val.data[,2] + (1-val.data[,i+2])*(1-val.data[,2])))
  ll.i <- ifelse(ll.i=="-Inf", sum(log(val.data[,i+2]+0.001)*val.data[,2] + log((1-val.data[,i+2]))*(1-val.data[,2])), ll.i)

  #summarize
  summary.i <- c(i,auc.i$AUC, corr.i, ll.i,kappa.opt[[2]], sens.i$sensitivity, spec.i$specificity, tss.i, kappa.i[[1]])
  summary.eval <- rbind(summary.eval, summary.i)
}
names(summary.eval) <- c("model", "auc", "corr", "ll", "threshold", "sens", "spec", "tss", "kappa")

#inspect
summary.eval

#add model names
summary.eval$model <- c("bio", "glm", "gam", "rf", "max")

#Calibration plots
calibration.plot(val.data,which.model=1, N.bins=5, xlab="Predicted", ylab="Observed", main="bioclim")
calibration.plot(val.data,which.model=2, N.bins=5, xlab="Predicted", ylab="Observed", main="glm")
calibration.plot(val.data,which.model=3, N.bins=5, xlab="Predicted", ylab="Observed", main="gam")
calibration.plot(val.data,which.model=4, N.bins=5, xlab="Predicted", ylab="Observed", main="rf")
calibration.plot(val.data,which.model=5, N.bins=5, xlab="Predicted", ylab="Observed", main="maxent")

#---------------------------------------#
#K-fold validation
#---------------------------------------#

#summary table for cross-validation with existing data
summary.eval.kfold <- data.frame(matrix(nrow=0, ncol=11))
names(summary.eval.kfold) <- c("model", "k", "auc", "corr", "ll", "boyce",
                             "threshold", "sens", "spec", "tss", "kappa")

folds <- 5 #number of k-folds considered

#create k-folds
kfold_pres <- kfold(pres.cov, k=folds)
kfold_back <- kfold(back.cov, k=folds)

for(k in 1:folds){

  #partition data into folds
  kfold <- k
  val.pres.k <- pres.cov[kfold_pres == kfold, ]
  val.back.k <- back.cov[kfold_back == kfold, ]
  val.k <- rbind(val.pres.k, val.back.k)
  val.k <- val.k[complete.cases(val.k[,"elev"]),]
  val.k.cov <- val.k[,cbind("canopy", "elev", "mesic1km", "precip")]

  train.pres.k <- pres.cov[kfold_pres != kfold, ]
  train.back.k <- back.cov[kfold_back != kfold, ]
  train.k <- rbind(train.pres.k, train.back.k)

  #models fit to fold
  bio.k <- bioclim(layers, train.pres.k[,1:2])
  glm.k <- glm(pres~canopy+elev+I(elev^2)+mesic1km+precip, family=binomial(link=logit), data=train.k)
  gam.k <- gam(pres~s(canopy)+s(elev)+s(mesic1km)+s(precip), family=binomial(link=logit), data=train.k)
  rf.k <- randomForest(as.factor(pres) ~ canopy+elev+mesic1km+precip, data=train.k, importance=F, ntree=500, mtry=1, na.action=na.omit)
  max.k <- maxent(layers, p=train.pres.k[,1:2], a=train.back.k[,1:2])

  #predictions for evaluation
  bio.val <- predict(bio.k,val.k.cov)
  glm.val <- predict(glm.k,val.k.cov, type="response")
  gam.val <- predict(gam.k,val.k.cov, type="response")
  rf.val <- predict(rf.k,val.k.cov, type="prob")
  rf.val <- rf.val[,2]
  max.val <- predict(max.k, val.k.cov)

  #evaluate model on fold
  val.data <- data.frame(siteID=1:nrow(val.k), obs=val.k$pres,
                        bio=bio.val, glm=glm.val, gam=gam.val, rf=rf.val, max=max.val)

  for(i in 1:nmodels){

    #calculate metrics for fold
    auc.i <- auc(val.data, which.model=i)
    kappa.opt <- optimal.thresholds(val.data, which.model=i, opt.methods=3)
    sens.i <- sensitivity(cmx(val.data, which.model=i,threshold = kappa.opt[[2]]))
    spec.i <- specificity(cmx(val.data, which.model=i,threshold = kappa.opt[[2]]))
    tss.i<- sens.i$sensitivity +spec.i$specificity - 1
    kappa.i <- Kappa(cmx(val.data, which.model=i,threshold = kappa.opt[[2]]))
    corr.i<-cor.test(val.data[,2],val.data[,i+2])$estimate
    ll.i <- sum(log(val.data[,i+2]*val.data[,2] + (1-val.data[,i+2])*(1-val.data[,2])))
    ll.i <- ifelse(ll.i=="-Inf", sum(log(val.data[,i+2]+0.001)*val.data[,2] + log((1-val.data[,i+2]))*(1-val.data[,2])),ll.i)
    boyce.i <- ecospat.boyce(fit=val.data[,i+2],obs=val.data[1:nrow(val.pres.k),i+2],res=100,PEplot = F)

    #summarize
    summary.i <- c(i,k,auc.i$AUC,corr.i,ll.i, boyce.i$Spearman.cor, kappa.opt[[2]],sens.i$sensitivity,spec.i$specificity,tss.i,kappa.i[[1]])
    summary.eval.kfold <- rbind(summary.eval.kfold, summary.i)
  }
  print(k)
}
names(summary.eval.kfold)<-c("model", "k", "auc", "corr", "ll",
                             "boyce", "threshold", "sens", "spec", "tss", "kappa")

#inspect
summary.eval.kfold

#average across folds
round(ddply(summary.eval.kfold, .(model), summarise,
      auc = mean(auc),
      cor = mean(corr),
      boyce = mean(boyce),
      tss = mean(tss),
      kappa = mean(kappa)
      ),3)

################################################
#7.3.5 Ensembles
################################################

#------------------------------------#
#weighted average based on AUC
#------------------------------------#

#create a raster stack from predictions
models <- stack(glm.map, gam.map)
names(models) <- c("glm", "gam")

#from prospective sampling
AUC.glm <- summary.eval[summary.eval$model=="glm", "auc"]
AUC.gam <- summary.eval[summary.eval$model=="gam", "auc"]

auc.weight <- c(AUC.glm, AUC.gam)

#AUC-based ensemble
ensemble.auc <- weighted.mean(models, auc.weight)

#plot
plot(ensemble.auc)

#------------------------------------#
#Frequency ensemble from binary maps
#------------------------------------#

#Get thresholds identified in PresenceAbsence
thres.glm <- summary.eval[summary.eval$model=="glm", "threshold"]
thres.gam <- summary.eval[summary.eval$model=="gam", "threshold"]

#Create binary maps
glm.thres.map <- glm.map
gam.thres.map <- gam.map

values(glm.thres.map) <- 0
values(gam.thres.map) <- 0
glm.thres.map[glm.map > thres.glm] <- 1
gam.thres.map[gam.map > thres.gam] <- 1

#plot
plot(glm.thres.map)
plot(gam.thres.map)

#Ensemble mapping based on frequency
ensemble.freq <- glm.thres.map + gam.thres.map

#plot
plot(ensemble.freq)

