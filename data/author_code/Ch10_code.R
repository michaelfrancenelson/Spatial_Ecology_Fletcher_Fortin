########################################################
########################################################
#Fletcher and Fortin 2019
#Chapter 10: Population dynamics in space
########################################################
########################################################

#load packages
library(ncf)               #for spline correlograms; version 1.2-5 used
library(unmarked)          #for dynamic occupancy models; version 0.12-2 used
library(reshape2)          #for re-formatting data; version 1.4.3 used
library(ggplot2)           #for some figures; version 3.0.0 used

#set working directory where data were downloaded
setwd(choose.dir())

##############################
#10.3.2 the data
##############################

#load data
surveys <- read.csv("orchid.csv")

#inspect
names(surveys)

#truncate N
surveys$presence <- ifelse(surveys$N > 0, 1, 0)

#reshape for all/secondary periods
surveys.occ <- dcast(surveys, siteID+x+y+z+phorophyte+area ~ survey_number, value.var="presence")
surveys.ab <- dcast(surveys, siteID+x+y+z+phorophyte+area ~ survey_number, value.var="N")

#reshape for primary periods
surveys.pri.occ <- dcast(surveys, siteID+x+y+z+phorophyte+area ~ primary_period, value.var="presence",max, fill=0)
surveys.pri.ab <- dcast(surveys, siteID+x+y+z+phorophyte+area ~ primary_period, value.var="N",max, fill=0)

#create detection / occurrence matrix
occ.matrix <- as.matrix(surveys.occ[,7:16])

#get occurrence in patches across entire period
occ.total <- apply(surveys.occ[,7:16], 1, max)

Nsites <- length(occ.total)
Noccupied <- sum(occ.total)

#plot
occ.color <- c("white","red")
plot(surveys.occ$x, surveys.occ$y,
     pch=21, bg=occ.color[as.factor(occ.total)],
     cex=log(surveys.occ$area+1)/4)

##################################
#10.3.3 spatial synchrony
##################################

#remove NAs
surveys.log.ab <- surveys.pri.ab
surveys.log.ab$Total <- rowSums(surveys.log.ab[,7:11])
surveys.log.ab <- subset(surveys.log.ab, Total>0)
surveys.log.ab[,7:11] <- log(surveys.log.ab[,7:11] + 1)

#population change
surveys.log.ab$g2001 <- surveys.log.ab$y2001 - surveys.log.ab$y2000
surveys.log.ab$g2002 <- surveys.log.ab$y2002 - surveys.log.ab$y2001
surveys.log.ab$g2003 <- surveys.log.ab$y2003 - surveys.log.ab$y2002
surveys.log.ab$g2004 <- surveys.log.ab$y2004 - surveys.log.ab$y2003


#266 sites (see mSynch, Sncf)
abund.synchrony1 <- spline.correlog(x=surveys.log.ab$x, y=surveys.log.ab$y, z=surveys.log.ab[,7],
                       xmax=200, resamp = 200)

#time series: log abundance
abund.synchrony <- Sncf(x=surveys.log.ab$x, y=surveys.log.ab$y, z=surveys.log.ab[,7:11],
                        xmax=200, resamp = 200)
#time series: growth
growth.synchrony <- Sncf(x=surveys.log.ab$x, y=surveys.log.ab$y, z=surveys.log.ab[,13:16],
                       xmax=200,resamp = 200)

#inspect
summary(abund.synchrony)
summary(abund.synchrony)$Regional.synch
summary(abund.synchrony)$Squantile

#plot
plot(abund.synchrony1)
plot(abund.synchrony)
plot(growth.synchrony)

##################################
#10.3.4 Metapopulation metrics
##################################

#patch area
area <- surveys.occ$area

#distance matrix
dist.matrix <- as.matrix(dist(cbind(surveys.occ$x, surveys.occ$y, surveys.occ$z)))

#maximum distance between patches
max(dist.matrix)

#define alpha
meandist <- 4.8
alpha <- 1/meandist

#ifm-like metric, ignoring occupancy
g <- exp(-alpha * dist.matrix)
diag(g) <- 0
g.sweep <- sweep(g, 2, area, "*")
S <- rowSums(g.sweep)

#ifm-like metric, including occupancy
Socc1 <- rowSums(g.sweep[,surveys.pri.occ$y2000 > 0])
Socc2 <- rowSums(g.sweep[,surveys.pri.occ$y2001 > 0])
Socc3 <- rowSums(g.sweep[,surveys.pri.occ$y2002 > 0])
Socc4 <- rowSums(g.sweep[,surveys.pri.occ$y2003 > 0])
Socc5 <- rowSums(g.sweep[,surveys.pri.occ$y2004 > 0])

round(cor(cbind(Socc1, Socc2, Socc3, Socc4, Socc5)),2)

#buffer metric
dist.binary <- ifelse(dist.matrix > meandist, 0, 1)
diag(dist.binary) <- 0
buffer <- rowSums(sweep(dist.binary, 2, area, "*"))

#make data frame and scale variables
site.cov <- data.frame(siteID=surveys.occ$siteID,
                       area=log(area+1), S=scale(S)[,1], buffer=scale(buffer)[,1])

################################################
#10.3.5: Estimating Colonization-Extinction
################################################

#-------------------------------------#
#Implict dynamics occupancy
#-------------------------------------#

dim(occ.matrix)
N <- nrow(occ.matrix)            #number of sites
J <- 2                           #number of secondary periods
Nprimary <- 5                    #number of primary periods

#re-format with reshape2
surveys.sec.occ <- dcast(surveys, siteID+x+y+z+phorophyte+primary_period ~ secondary_period, value.var="presence")
surveys.sec.occ <- merge(site.cov, surveys.sec.occ, by="siteID")

#scale
surveys.sec.occ$area <- log(surveys.sec.occ$area + 1)
surveys.sec.occ$S <- scale(surveys.sec.occ$S)[,1]
surveys.sec.occ$buffer <- scale(surveys.sec.occ$buffer)[,1]

#get date
surveys$day <- as.POSIXlt(strptime(surveys$date, "%m/%d/%Y"))
surveys$julian <- scale(surveys$day$yday)[,1]
date <- dcast(surveys, siteID+primary_period ~ secondary_period, value.var="julian")

#create unmarked object
occ.data <- unmarkedFrameOccu(y=surveys.sec.occ[,10:11],
                           siteCovs=surveys.sec.occ[,c(2:4,8:9)],
                           obsCovs=list(date=date[,3:4]))

#inspect
summary(occ.data)

#first tilde: p, second: psi
occ.p.int <- occu(~1 ~1, occ.data)
occ.p.date <- occu(~date ~1, occ.data)
occ.p.area <- occu(~area ~1, occ.data)
occ.p.datearea <- occu(~date+area ~1, occ.data)

#combine models into a list
model.p.list <- fitList( "p.null"=occ.p.int,
                           "p.area"=occ.p.area,
                           "p.date"=occ.p.date,
                           "p.datearea"=occ.p.datearea)

#Model selection table
modSel(model.p.list)

#inspect
summary(occ.p.datearea)

#Without primary period as covariate
occ.null.p.datearea <- occu(~date+area ~1, occ.data)
occ.phoro.p.datearea <- occu(~date+area ~phorophyte, occ.data)
occ.area.p.datearea <- occu(~date+area ~area, occ.data)
occ.S.p.datearea <- occu(~date+area ~S, occ.data)
occ.buffer.p.datearea <- occu(~date+area ~buffer, occ.data)
occ.areaS.p.datearea <- occu(~date+area ~area+S, occ.data)
occ.areabuffer.p.datearea <- occu(~date+area ~area+buffer, occ.data)

model.occ.list <- fitList("null"=occ.null.p.datearea,
                          "phoro"= occ.phoro.p.datearea,
                          "area"=occ.area.p.datearea,
                          "S"=occ.S.p.datearea,
                          "buffer"=occ.buffer.p.datearea,
                          "area+S"=occ.areaS.p.datearea,
                          "area+buffer"=occ.areabuffer.p.datearea)

#inspect
modSel(model.occ.list)

#best model
summary(occ.areaS.p.datearea)

#-------------------------------------#
#dynamic occupancy
#-------------------------------------#

#format
Nprimary <- length(levels(surveys$primary_period))
date.wide <- dcast(surveys, siteID~survey_number, value.var="julian")
primary.cov <- data.frame(Socc=scale(c(Socc1, Socc2, Socc3, Socc4, Socc5))[,1])

#must specify date in list form
DO.data<-unmarkedMultFrame(y=occ.matrix,
                           siteCovs=site.cov,
                           obsCovs=list(date=date.wide[,2:11]),
                           yearlySiteCovs=primary.cov,
                           numPrimary=Nprimary)
#inspect
summary(DO.data)

#models where psi1 is constant
DO.psi.int.col.int.eps.int.p.int <- colext(psiformula= ~1, gammaformula =  ~ 1, epsilonformula = ~ 1,
                                   pformula = ~ 1, DO.data)
DO.psi.int.col.int.eps.int <- colext(psiformula= ~1, gammaformula =  ~ 1, epsilonformula = ~ 1,
                                     pformula = ~ date+area, DO.data)
DO.psi.int.col.int.eps.area <- colext(psiformula= ~1, gammaformula =  ~ 1, epsilonformula = ~ area,
                                      pformula = ~ date+area, DO.data)
DO.psi.int.col.int.eps.areaS <- colext(psiformula= ~1, gammaformula =  ~ 1, epsilonformula = ~ S+area,
                                       pformula = ~ date+area, DO.data)
DO.psi.int.col.S.eps.int <- colext(psiformula= ~1, gammaformula =  ~ S, epsilonformula = ~ 1,
                                   pformula = ~ date+area, DO.data)
DO.psi.int.col.S.eps.area <- colext(psiformula= ~1, gammaformula =  ~ S, epsilonformula = ~ area,
                                    pformula = ~ date+area, DO.data)

#models where psi1 depends on parameters
DO.psi.areaS.col.int.eps.int <- colext(psiformula= ~area+S, gammaformula =  ~ 1, epsilonformula = ~ 1,
               pformula = ~ date+area, DO.data)
DO.psi.areaS.col.S.eps.area <- colext(psiformula= ~area+S, gammaformula =  ~ S, epsilonformula = ~ area,
                                  pformula = ~ date+area, DO.data)
DO.psi.areaS.col.int.eps.area <- colext(psiformula= ~area+S, gammaformula =  ~ 1, epsilonformula = ~ area,
                                  pformula = ~ date+area, DO.data)
DO.psi.areaS.col.int.eps.areaS <- colext(psiformula= ~area+S, gammaformula =  ~ 1, epsilonformula = ~ S+area,
                                    pformula = ~ date+area, DO.data)
DO.psi.areaS.col.S.eps.int <- colext(psiformula= ~area+S, gammaformula =  ~ S, epsilonformula = ~ 1,
                                   pformula = ~ date+area, DO.data)

model.DO.list <- fitList(
                         "psi-constant,null"=DO.psi.int.col.int.eps.int,
                         "psi-constant,ext-area"=DO.psi.int.col.int.eps.area,
                         "psi-constant,ext-area+S"=DO.psi.int.col.int.eps.areaS,
                         "psi-constant,ext-area, col-S"=DO.psi.int.col.S.eps.area,
                         "psi-constant, col-S"=DO.psi.int.col.S.eps.int,

                         "psi-area+S,null"=DO.psi.areaS.col.int.eps.int,
                         "psi-area+S,ext-area"=DO.psi.areaS.col.int.eps.area,
                         "psi-area+S,ext-area+S"=DO.psi.areaS.col.int.eps.areaS,
                         "psi-area+S,ext-area, col-S"=DO.psi.areaS.col.S.eps.area,
                         "psi-area+S, col-S"=DO.psi.areaS.col.S.eps.int
                         )

#model selection table
modSel(model.DO.list)

#summary of most supported model
summary(DO.psi.areaS.col.int.eps.area)

#backtransform from link to probability scale
backTransform(DO.psi.int.col.int.eps.int.p.int, type="det")
backTransform(DO.psi.int.col.int.eps.int.p.int, type="col")
backTransform(DO.psi.int.col.int.eps.int.p.int, type="ext")
backTransform(DO.psi.int.col.int.eps.int.p.int, type="psi")

#-----------------------#
#predictions
#-----------------------#

#create new data frame for predictions
Srange <- seq(min(site.cov$S), max(site.cov$S), length=20)
Smean <- mean(site.cov$S)
Arearange <- seq(min(site.cov$area), max(site.cov$area), length=20)
Areamean <- mean(site.cov$area)
Datemean <- mean(as.matrix(date.wide[,2:11]))

#make data frame
newdata.iso <- expand.grid(area=Areamean, S=Srange, date=Datemean)
newdata.area <- expand.grid(area=Arearange, S=Smean, date=Datemean)

#extinction prediction
ext.pred <- predict(DO.psi.areaS.col.int.eps.area, newdata.area, type='ext')
newdata.ext <- cbind(newdata.area, ext.pred)

#inspect
head(newdata.ext)

#back-transform area
newdata.ext$areaback <- exp(newdata.ext$area)-1

#plot
plot(newdata.ext$areaback, newdata.ext$Predicted, ylim=c(0,0.5))
lines(newdata.ext$areaback, newdata.ext$lower)
lines(newdata.ext$areaback, newdata.ext$upper)

#psi1 area
psi.area.pred <- predict(DO.psi.areaS.col.int.eps.area, newdata.area, type='psi')
newdata.psi.area <- cbind(newdata.area,psi.area.pred)
newdata.psi.area$areaback <- exp(newdata.psi.area$area)-1   #back-transform area

#inspect
head(newdata.psi.area)

#plot psi1
plot(newdata.psi.area$areaback, newdata.psi.area$Predicted,ylim=c(0,1))
lines(newdata.psi.area$areaback, newdata.psi.area$lower)
lines(newdata.psi.area$areaback, newdata.psi.area$upper)

#psi S
psi.iso.pred <- predict(DO.psi.areaS.col.int.eps.area, newdata.iso, type='psi')
newdata.psi.iso <- cbind(newdata.iso, psi.iso.pred)

#inspect
head(newdata.psi.iso)

#######################################################
#10.3.6 Projecting dynamics
#######################################################

#predict onto original site covariates to get site-specific col-ext
newdata.fit <- data.frame(site.cov, date=Datemean)

#predict onto new data
ext.fit <- predict(DO.psi.areaS.col.int.eps.area, newdata.fit, type='ext')
col.fit <- predict(DO.psi.areaS.col.int.eps.area, newdata.fit, type='col')
psi.fit <- predict(DO.psi.areaS.col.int.eps.area, newdata.fit, type='psi')
col.pred <- col.fit$Predicted
ext.pred <- ext.fit$Predicted
psi1.pred <- psi.fit$Predicted

#simulation parameters
timeframe <- 100     #time steps to simulate
reps <- 20           #realizations

#projection function
colext.sim<-function(occ.int, col, ext, timeframe, reps)
{
  Nsite <- length(occ.int)
  colext.out <- array(NA,dim=c(reps, Nsite, timeframe))

  #z: time1
  colext.out[,,1] <- rbinom(Nsite, prob=occ.int, size=1)

  #z: time2-T
  for(j in 1:reps){
    for(t in 2:timeframe){
      colext.jt <- colext.out[j,,t-1] * (1-ext) + (1-colext.out[j,,t-1]) * col
      colext.out[j,,t] <- rbinom(Nsite, prob=colext.jt, size=1)
    }#end i
  }#end j

  return(colext.out)
}

colext.proj <- colext.sim(occ.int=psi1.pred, col=col.pred, ext=ext.pred, timeframe=timeframe, reps=reps)

#inspect
dim(colext.proj)
head(colext.proj[1,,])

#site-specific means across realizations
colext.patch.mean <- apply(colext.proj, c(1,2), mean)
colext.patch.mean <- t(colext.patch.mean)

#inspect
dim(colext.patch.mean)

#landscape mean over time
colext.land.mean <- apply(colext.proj, 1, mean)

#inspect
colext.land.mean

###################################################
#10.3.7 Metapopulation viability
###################################################

#metapopulation capacity function
meta.cap <- function(A, distmat)
{
  M <- outer(A, A) * distmat
  tmp <- eigen(M)
  vector.M <- Re(tmp$vector[, 1]^2)      #Patch-specific eigenvectors
  lambda.M <- Re(tmp$value[1])           #Metapopulation capacity
  return(list(lambda.M, vector.M))
}

#calculate metapopulation capacity
metacap <- meta.cap(A=area, distmat=dist.matrix)

#inspect
str(metacap)
metacap[[1]]

#Simulating random habitat loss and resulting metapopulation capacity
reps <- 20                                  #20 realizations
Npatches <- length(surveys.occ$siteID)      #number of patches
metacap.rand <- matrix(0, 10, reps)         #matrix for storing output

for (z in 1:reps){

  rand <- sample(surveys.occ$siteID)        #shuffles in rand order

  #randomly remove patches, 10/time step
  for (i in 0:9){

      removal.percent <- i*10
      N.removal <- round(removal.percent * length(rand)/100,0)
      patch.removals <- rand[1:N.removal]

      patch.i <- surveys.occ[!(surveys.occ$siteID%in%patch.removals),]
      area.i <- patch.i$area
      dist.i <- as.matrix(dist(cbind(patch.i$x, patch.i$y, patch.i$z)))
      metacap.i <- meta.cap(A=area.i, distmat=dist.i)
      metacap.rand[i,z] <- metacap.i[[1]]
    }#for i
  print(z)
}#for z

#inspect
dim(metacap.rand)
head(metacap.rand)

#create data frame
metacap.rand.df <- data.frame(loss=seq(0, 90, by=10), metacap.rand)

#from wide to long format
metacap.rand.melt <- melt(metacap.rand.df, id.var="loss")

#inspect
head(metacap.rand.melt)
names(metacap.rand.melt) <- c("Loss","Rep", "metacap")

#plot with ggplot2
ggplot(metacap.rand.melt, aes(x=Loss, y=metacap, colour=Rep))+
  geom_line(aes(group=Rep)) +
  xlab("Random Patch Loss") + ylab("Metapopulation capacity")+
  xlim(c(0,100))+
  theme_bw()+
  theme(axis.title = element_text(vjust=-0.5, size=16, colour = "black"),
        axis.text = element_text(vjust=0.5, size=12, colour = "black"),
        legend.position = "none")


