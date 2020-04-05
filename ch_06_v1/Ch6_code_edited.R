#Fletcher and Fortin 2019
#Chapter 6: Accounting for Spatial Dependence in Ecological Data

#load packages ----
library(raster)           #for raster covariate data; version 2.6-7 used
library(spdep)            #for general correlograms; version 0.7-8 used
library(lme4)             #for multi-level models; version 1.1-18-1 used
library(vegan)            #for eigenvector mapping; version 2.5-2 used
library(mgcv)             #for gam; version 1.8-24 used
library(MASS)             #for spatial gls; version 7.3-50 used
library(spaMM)            #for spatial glmm; version 2.4-35 used
library(deldir)           #to create a lattice/theissen polygons; version 0.1-15 used
library(dismo)            #to create a lattice/theissen polygons; version 1.1-4 used

dnearneigh()
#to install INLA
# install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
library(INLA)             #Bayesian CAR spatial model; version 17.06.20 used

#set working directory where data were downloaded
setwd(choose.dir())

#load functions needed
icorrelogram <- function(locations, z, binsize, maxdist){
  
  distbin <- seq(0, maxdist, by = binsize)
  Nbin <- length(distbin)-1
  moran.results <- data.frame("dist" =  rep(NA, Nbin), "Morans.i" = NA, "null.lower" = NA, "null.lower" = NA)
  
  for (i in 1:Nbin){
    d.start <- distbin[i] 
    d.end <- distbin[i + 1]
    neigh <- dnearneigh(x = locations, d1 = d.start, d.end, longlat = F)
    wts <- nb2listw(neighbours = neigh, style = 'B', zero.policy = T)
    mor.i <- moran.mc(x = z, listw = wts, nsim = 200, alternative = "greater", zero.policy = T)  #note alternative is for P-value, so only 'significant if positive autocorrelation
    
    moran.results[i, "dist"] <- (d.end + d.start)/2 
    moran.results[i, "Morans.i"] <- mor.i$statistic 								                #observed moran's i
    moran.results[i, "null.lower"] <- quantile(mor.i$res, probs = 0.025, na.rm = T)#95% null envelope	
    moran.results[i, "null.upper"] <- quantile(mor.i$res, probs = 0.975, na.rm = T)#95% null envelope
  }
  return(moran.results)
}

# source('Ch6_functions.r')

# Inspect function
icorrelogram

#----------------------------------#
#6.3.3 Models that ignore spatial dependence ----
#----------------------------------#

elev <- raster("elev.gri")
proj4string(elev)

#------------------------------------------#
#CRS:
#albers conic equal area
#Projection Albers
#gcs_north_american_1983
#Spheroid: GRS_1980

#Geographic coordinate system (datum):
#  NAD83 Unit of measure: meters Projection:
#  Albers Central Meridian: -109.5
#  Standard Parallel 1: 46
#  Standard Parallel 2: 48
#  Latitude of Origin: 44
#  False Easting: 600000
#  False Northing: 0
#------------------------------------------#

elev.crs <- CRS(" + proj = aea  + lat_1 = 46  + lat_2 = 48  + lat_0 = 44  + lon_0 = -109.5
               + x_0 = 600000  + y_0 = 0  + ellps = GRS80  + datum = NAD83  + units = m  + no_defs")
proj4string(elev) <- elev.crs

#inspect
proj4string(elev)

#create slope, aspect layers from elevation
elev.terr <- terrain(elev, opt = c("slope", "aspect"))

#make a multilayered file for extraction
layers <- stack(elev, elev.terr)
names(layers) <- c("elev", "slope", "aspect")

#plot
plot(layers)

#Point data with presence/absence
point.data <- read.csv(file.path(book_data, "vath_2004.csv"), header = TRUE)

#plot
plot(elev)
points(point.data[point.data$VATH =  = 1, c("EASTING", "NORTHING")], col = "red")      #plots the presence points
points(point.data[point.data$VATH =  = 0, c("EASTING", "NORTHING")], col = "black")    #plots the absence points

#extract GIS data at sampling points
coords <- cbind(point.data$EASTING, point.data$NORTHING)
land.cov <- extract(x = layers, y = coords)

#combine
point.data <- cbind(point.data, land.cov)

#inspect
head(point.data)



#----------------------------------#
#Logistic regression ignoring spatial dependence ----
#----------------------------------#

#check correlations among predictor variables
cor(point.data[, c("elev", "slope", "aspect")], method = "pearson")
pairs(point.data[, c("elev", "slope", "aspect")])

#center and scale predictors for modeling
point.data$elevs <- scale(point.data$elev, center = T, scale = T)
point.data$slopes <- scale(point.data$slope, center = T, scale = T)
point.data$aspects <- scale(point.data$aspect, center = T, scale = T)

#logistic regression on elev
VATH.elev <- glm(VATH ~ elev, family = "binomial", data = point.data)
summary(VATH.elev)

#logistic regression with all additive factors
VATH.all <- glm(VATH ~ elevs + slopes + aspects, family = "binomial", data = point.data)
summary(VATH.all)

#logistic regression with elevation as a quadratic (nonlinear) effect
VATH.elev2 <- glm(VATH ~ elev + I(elev^2), family = "binomial", data = point.data)
summary(VATH.elev2)

#contrast models with AIC
AIC(VATH.elev, VATH.all, VATH.elev2)

#extract coefficients and their SEs from the best model
summary(VATH.elev2)$coef
glm.summary <- c(summary(VATH.elev2)$coef[2, 1], 
                 summary(VATH.elev2)$coef[2, 2], 
                 summary(VATH.elev2)$coef[3, 1], 
                 summary(VATH.elev2)$coef[3, 2])
#inspect
glm.summary



#plot of predicted relationship ----

#first create a new data set
Elev <-  seq(min(point.data$elev), max(point.data$elev), length = 15)
newdata <- data.frame(elev = Elev)

#Predict onto newdata
#type = response for predicted probabilities
glm.pred <- predict(VATH.elev2, newdata = newdata, type =  "link", se = T) 
ucl <- glm.pred$fit + 1.96*glm.pred$se.fit
lcl <- glm.pred$fit - 1.96*glm.pred$se.fit

#back-transform from link to probability scale
glm.newdata <- data.frame(newdata, pred = plogis(glm.pred$fit), lcl = plogis(lcl), ucl = plogis(ucl))

#plot
plot(glm.newdata$elev, glm.newdata$pred, ylim = c(0, 0.5), xlab = "Elevation", ylab = "Prob. Occurrence")
lines(glm.newdata$elev, glm.newdata$lcl)
lines(glm.newdata$elev, glm.newdata$ucl)

#Map the model
glm.raster <- predict(model = VATH.elev2, object = layers,  type = "response")
plot(glm.raster, xlab = "Longitude", ylab = "Latitude")

#----------------------------------#
# Inspect spatial dependence of response variable ----
#----------------------------------#

#run function I provided for an indicator correlogram
VATH.cor <- icorrelogram(locations = coords, z = point.data$VATH, binsize = 1000, maxdist = 15000)

#inspect
round(head(VATH.cor, 3), 2)

#plot correlogram
plot(VATH.cor$dist, VATH.cor$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(VATH.cor$dist, VATH.cor$null.lcl)
lines(VATH.cor$dist, VATH.cor$null.ucl)

#----------------------------------#
#Inspect spatial dependence of GLM residuals ----
#----------------------------------#

#residuals from quadratic elevation model
VATH.elev2.res <- residuals(VATH.elev2, type = "deviance")

#correlogram on residuals
corr.res <- icorrelogram(locations = coords, z = VATH.elev2.res, binsize = 1000, maxdist = 15000)

#plot correlogram
plot(corr.res$dist, corr.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(corr.res$dist, corr.res$null.lcl)
lines(corr.res$dist, corr.res$null.ucl)

#contrast results from raw to residuals with mean
VATH.int <- glm(VATH ~ 1, family = "binomial", data = point.data)
VATH.int.res <- residuals(VATH.int, type = "deviance")

#correlogram on residuals of mean model (intercept model)
corr.int.res <- icorrelogram(locations = coords, z = VATH.int.res, binsize = 1000, maxdist = 15000)

#correlation
cor(VATH.cor$Morans.i, corr.int.res$Morans.i)




# Subset data to account for autocorrelation ----

#randomly shuffle data by transect and create a shuffled rank vector
rand.vector <- with(
  point.data, 
  ave(
    POINT, 
    as.factor(TRANSECT), 
    FUN = function(x) {sample(length(x))}))

#pick one random point on transect and remove rest
point.datasub <- point.data[rand.vector  <= 1, ]

#coordinates subset
coords.sub <- cbind(point.datasub$NORTHING, point.datasub$NORTHING)

#model
VATH.sub <- glm(VATH~elev + I(elev^2), family = "binomial", data = point.datasub)
summary(VATH.sub)

#extract coefficients
glmsub.summary <- c(summary(VATH.sub)$coef[2, 1], 
                    summary(VATH.sub)$coef[2, 2], 
                    summary(VATH.sub)$coef[3, 1], 
                    summary(VATH.sub)$coef[3, 2])

#residuals
VATH.sub.res <- residuals(VATH.sub, type = "deviance")

#correlogram on residuals
corr.sub.res <- icorrelogram(locations = coords.sub, z = VATH.sub.res, binsize = 2000, maxdist = 15000)

#plot correlogram
plot(corr.sub.res$dist, corr.sub.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(corr.sub.res$dist, corr.sub.res$null.lcl)
lines(corr.sub.res$dist, corr.sub.res$null.ucl)



#----------------------------------#
#6.3.4.1 Trend surface models ----
#----------------------------------#

#Polynomial trend surface model ----

VATH.trend <- glm(VATH~elev + I(elev^2) + EASTING + NORTHING + I(EASTING^2) + 
                    I(EASTING^3) + I(NORTHING^2) + I(NORTHING^3), family = "binomial", data = point.data)

#inspect
summary(VATH.trend)

#extract coefficients
trend.summary <- c(summary(VATH.trend)$coef[2, 1], 
                   summary(VATH.trend)$coef[2, 2], 
                   summary(VATH.trend)$coef[3, 1], 
                   summary(VATH.trend)$coef[3, 2])

#extract residuals
VATH.trend.res <- residuals(VATH.trend, type = "deviance")

#correlogram on residuals
cor.trend.res <- icorrelogram(locations = coords, z = VATH.trend.res, binsize = 1000, maxdist = 15000)

#plot correlogram
plot(cor.trend.res$dist, cor.trend.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(cor.trend.res$dist, cor.trend.res$null.lcl)
lines(cor.trend.res$dist, cor.trend.res$null.ucl)



# GAM trend surface model ----

VATH.gam <- gam(VATH~elev + I(elev^2) + s(EASTING, NORTHING), family = "binomial", data = point.data)

#inspect
summary(VATH.gam)

#extract coefficients
gam.summary <- c(summary(VATH.gam)$p.coeff[2], 
                 summary(VATH.gam)$se[2], 
                 summary(VATH.gam)$p.coeff[3], 
                 summary(VATH.gam)$se[3])

#residuals
VATH.gam.res <- residuals(VATH.gam, type = "deviance")

#correlogram on residuals
cor.gam.res <- icorrelogram(locations = coords, z = VATH.gam.res, binsize = 1000, maxdist = 15000)

#plot correlogram
plot(cor.gam.res$dist, cor.gam.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(cor.gam.res$dist, cor.gam.res$null.lcl)
lines(cor.gam.res$dist, cor.gam.res$null.ucl)



#----------------------------------#
#6.3.4.2 Eigenvector mapping ----
#----------------------------------#

?ME

#get threshold distance based on minimum spanning tree
spantree.em <- spantree(dist(coords), toolong = 0)
max(spantree.em$dist)

#create neighbhorhood matrix
dnn <-  dnearneigh(coords, 0, max(spantree.em$dist))                   #list of links to neighboring points within distance range
dnn_dists <- nbdists(dnn, coords)                                    #list of distances (based on links in dnn)
dnn_sims <- lapply(dnn_dists, function(x) (1-((x/4)^2)))             #scale distances as recommended by Dormann et al. (2007)
ME.weight <- nb2listw(dnn, glist = dnn_sims, style = "B", zero.policy = T) #create spatial weights matrix (in list form)

#eigenvector model selection
VATH.ME <- ME(VATH~elev + I(elev^2), listw = ME.weight, family = "binomial", data = point.data)

#inspect
summary(VATH.ME)

#eigenvectors selected
head(VATH.ME$selection)
head(fitted(VATH.ME), 2)
head(VATH.ME$vectors)

#new glm with ME covariates
VATH.evm <- glm(VATH~elev + I(elev^2) + fitted(VATH.ME), family = "binomial", data = point.data)

#inspect
summary(VATH.evm)

#extract coefficients
evm.summary <- c(summary(VATH.evm)$coef[2, 1], 
                 summary(VATH.evm)$coef[2, 2], 
                 summary(VATH.evm)$coef[3, 1], 
                 summary(VATH.evm)$coef[3, 2])

#residuals
VATH.evm.res <- residuals(VATH.evm, type = "deviance")

#correlogram on residuals
cor.evm.res <- icorrelogram(locations = coords, z = VATH.evm.res, binsize = 1000, maxdist = 15000)

#plot correlogram
plot(cor.evm.res$dist, cor.evm.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(cor.evm.res$dist, cor.evm.res$null.lcl)
lines(cor.evm.res$dist, cor.evm.res$null.ucl)

#plot eigenvector by x, then y
plot(point.data$EASTING, fitted(VATH.ME)[, 1])
plot(point.data$NORTHING, fitted(VATH.ME)[, 1])

#Plot eigenvectors in x, y
plot(point.data[, c("EASTING", "NORTHING")], pch = 21, col = "black", cex = 2, lwd = 0.5, 
     bg = topo.colors(6)[cut(fitted(VATH.ME)[, 1], breaks = 6)], main = "vec796")
plot(point.data[, c("EASTING", "NORTHING")], pch = 21, col = "black", cex = 2, lwd = 0.5, 
     bg = topo.colors(6)[cut(fitted(VATH.ME)[, 2], breaks = 6)], main = "vec804")
plot(point.data[, c("EASTING", "NORTHING")], pch = 21, col = "black", cex = 2, lwd = 0.5, 
     bg = topo.colors(6)[cut(fitted(VATH.ME)[, 3], breaks = 6)], main = "vec805")

#Overlay eigenvectors on predicted map
plot(glm.raster, xlab = "Longitude", ylab = "Latitude")
points(point.data[, c("EASTING", "NORTHING")], pch = 21, col = "black", cex = 2, lwd = 0.5, 
       bg = topo.colors(6)[cut(fitted(VATH.ME)[, 1], breaks = 6)])



#----------------------------------#
#6.3.4.3 autocovariate logistic reg ----
#----------------------------------#

#Create alternative autocovariates with 1km radius
auto1km_inv <- autocov_dist(point.data$VATH, coords, nbs = 1000, type =  "inverse", zero.policy = T)    #inverse of distance weights
auto1km <- autocov_dist(point.data$VATH, coords, nbs = 1000, type =  "one", style = "B", zero.policy = T)  #binary weights

#Contrast autocovariates with 1km radius
cor(auto1km, auto1km_inv)

#plot
plot(auto1km, auto1km_inv)
plot(jitter(auto1km, factor = 0.4), auto1km_inv) #x-axis jittered to better see points

#autocovariate model
VATH.auto1km <- glm(VATH~elev + I(elev^2) + auto1km, family = "binomial", data = point.data)

#inspect
summary(VATH.auto1km)

#extract coefficients
auto.summary <- c(summary(VATH.auto1km)$coef[2, 1], 
                  summary(VATH.auto1km)$coef[2, 2], 
                  summary(VATH.auto1km)$coef[3, 1], 
                  summary(VATH.auto1km)$coef[3, 2])

#residuals
VATH.auto.res <- residuals(VATH.auto1km, type = "deviance")

#correlogram on residuals
cor.auto.res <- icorrelogram(locations = coords, z = VATH.auto.res, binsize = 1000, maxdist = 15000)

#plot correlogram
plot(cor.auto.res$dist, cor.auto.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(cor.auto.res$dist, cor.auto.res$null.lcl)
lines(cor.auto.res$dist, cor.auto.res$null.ucl)




#----------------------------------#
# 6.3.4.4 CAR models ----
#----------------------------------#

#neighborhood based on tesselation:
theissen <- voronoi(coords)

#plot
spplot(theissen, "id")

#or plot
plot(theissen)
points(coords, cex = 0.5, col = "red")

#plot neighborhood matrix as links
point.poly <- poly2nb(theissen)
plot(point.poly, coords, col = "red", add = T)

#create adjacency matrix
adj <- nb2mat(point.poly, style = "B")     #binary weights matrix based on neighbor polygons
adj <- as(adj, "dgTMatrix")                #make it a 'sparse' matrix

#add unique identifier for observations
point.data$id <- 1:nrow(point.data)

#intrinsic CAR (ICAR)
VATH.inla <- inla(VATH ~ elev + I(elev^2) +  f(id, model = "besag", graph = adj), 
                  family = "binomial", data = point.data, control.predictor = list(compute = T))

#inspect
summary(VATH.inla)
summary(VATH.inla)$fixed[2, 1]

#estimates
inla.summary <- c(summary(VATH.inla)$fixed[2, 1], 
                  summary(VATH.inla)$fixed[2, 2], 
                  summary(VATH.inla)$fixed[3, 1], 
                  summary(VATH.inla)$fixed[3, 2])

#calculate deviance residuals manually
VATH.inla.fit <- VATH.inla$summary.fitted.values$mean
si <- ifelse(point.data$VATH =  = 1, 1, -1)

VATH.inla.res <- si*(-2*(point.data$VATH*log(VATH.inla.fit) + 
                           (1-point.data$VATH)*log(1-VATH.inla.fit)))^0.5#deviance residual

#correlogram on residuals
cor.inla.res <- icorrelogram(locations = coords, z = VATH.inla.res, binsize = 1000, maxdist = 15000)

#plot correlogram
plot(cor.inla.res$dist, cor.inla.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(cor.inla.res$dist, cor.inla.res$null.lcl)
lines(cor.inla.res$dist, cor.inla.res$null.ucl)



#----------------------------------#
#6.3.4.5 Multi-level model ----
#----------------------------------#

#random effects should be a factor
str(point.data)
point.data$TRANSECT <- as.factor(point.data$TRANSECT) #need for transect to be considered a factor

#glmm using lme4
VATH.glmm <- glmer(VATH~elev + I(elev^2) + (1|TRANSECT), family = "binomial", data = point.data)

#inspect
summary(VATH.glmm)

#extract coefficients
glmm.summary <- c(summary(VATH.glmm)$coef[2, 1], 
                  summary(VATH.glmm)$coef[2, 2], 
                  summary(VATH.glmm)$coef[3, 1], 
                  summary(VATH.glmm)$coef[3, 2])

#residuals
VATH.glmm.res <- resid(VATH.glmm)

#correlogram on residuals
cor.glmm.res <- icorrelogram(locations = coords, z = VATH.glmm.res, binsize = 1000, maxdist = 15000)

#plot correlogram
plot(cor.glmm.res$dist, cor.glmm.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(cor.glmm.res$dist, cor.glmm.res$null.lcl)
lines(cor.glmm.res$dist, cor.glmm.res$null.ucl)




#----------------------------------#
#6.3.4.6 spatial GLS/GLMM ----
#----------------------------------#

#spatial GLMM correlation only within transects using MASS package
VATH.pql <- glmmPQL(VATH~elev + I(elev^2), random = ~1|TRANSECT, 
                    correlation =  corExp(form = ~ EASTING + NORTHING), 
                    family = "binomial", data = point.data)

#spatial GLS correlation across entire area (EXTENT) using MASS package; ~time 371 s
GROUP <- factor(rep("obs", nrow(point.data)))
VATH.gls <- glmmPQL(VATH~elev + I(elev^2), random = ~1|GROUP, 
                    correlation =  corExp(form = ~ EASTING + NORTHING), 
                    family = "binomial", data = point.data)

#inspect
summary(VATH.pql)
summary(VATH.gls)
summary(VATH.pql)$tTable[2, 2]#provides coefficients shown in summary
summary(VATH.gls)$tTable[2, 2]#provides coefficients shown in summary

#plot estimated spatial covariance
plot.dist <- seq(1, 5000, by = 100)
plot.gls.exp <- exp(-plot.dist/212.9) #estimated range = 212.9m
plot(plot.dist, plot.gls.exp, xlab = "Distance", ylab = "Correlation")

#extract coefficients
gls.summary <- c(summary(VATH.gls)$tTable[2, 1], 
                 summary(VATH.gls)$tTable[2, 2], 
                 summary(VATH.gls)$tTable[3, 1], 
                 summary(VATH.gls)$tTable[3, 2])

#residuals: won't generate deviance residuals/only response/pearson
VATH.gls.res <- residuals(VATH.gls, type = "response")

#correlogram on residuals
cor.gls.res <- icorrelogram(locations = coords, z = VATH.gls.res, binsize = 1000, maxdist = 15000)

#plot correlogram
plot(cor.gls.res$dist, cor.gls.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(cor.gls.res$dist, cor.gls.res$null.lcl)
lines(cor.gls.res$dist, cor.gls.res$null.ucl)




#Alternative spatial glmm ----

#ML estimation with spaMM, but SLOW (20252 s on my computer); may require scaling elevation for convergence
system.time(VATH.spamm.ml <- corrHLfit(VATH~elev + I(elev^2) + Matern(1|EASTING + NORTHING), 
                                       HLmethod = "ML", data = point.data, 
                                       family = binomial(), ranFix = list(nu = 0.5)))

#inspect
summary(VATH.spamm.ml)

#residuals
VATH.spamm.res <- dev_resids(VATH.spamm.ml)

#correlogram on residuals
cor.spamm.res <- icorrelogram(locations = coords, z = VATH.spamm.res, binsize = 1000, maxdist = 15000)

#estimates
spamm.summary <- c(summary(VATH.spamm.ml)$beta_table[2, 1], 
                   summary(VATH.spamm.ml)$beta_table[2, 2], 
                   summary(VATH.spamm.ml)$beta_table[3, 1], 
                   summary(VATH.spamm.ml)$beta_table[3, 2])

#plot correlogram
plot(cor.spamm.res$dist, cor.spamm.res$Morans.i, ylim = c(-0.5, 0.5))
abline(h = 0, lty = "dashed")
lines(cor.spamm.res$dist, cor.spamm.res$null.lcl)
lines(cor.spamm.res$dist, cor.spamm.res$null.ucl)



# Contrast estimates ----

#Summarize coefficients/SEs
model.coeff <- data.frame(rbind(glm.summary, glmsub.summary, trend.summary, gam.summary, 
                                auto.summary, evm.summary, glmm.summary, gls.summary, inla.summary))
model.coeff$model <- c("glm", "glmsub", "trend", "gam", "auto", "evm", "glmm", "gls", "CAR")
names(model.coeff) <- c("elev", "elev.SE", "elev2", "elev2.SE", "model")
model.coeff
