########################################################
########################################################
#Fletcher and Fortin 2018
#Ch 11: Spatially structured communities
########################################################
########################################################

#load packages
library(raster)           #for raster covariate data; version 2.6-7 used
library(rgdal)            #for reading different types of GIS files; version 1.3-4 used
library(gdm)              #generalized dissimilarity modeling; version 1.3.11 used
library(reshape2)         #for re-formatting data; version 1.4.3 used
library(lme4)             #for multi-level models; version 1.1-18-1 used
library(vegan)            #for eigenvector mapping; version 2.5-2 used
library(MASS)             #for NB model; version 7.3-50 used
library(AER)              #for overdispersion tests; version 1.2-5 used

#set working directory where data were downloaded
setwd(choose.dir())

#increase memory
mem.max <- memory.limit(size=NA)
memory.limit(size=mem.max)

################################################
#11.3.3 Modeling communities
################################################

#layers
Elev <- raster("elev.gri")                           #elevation layer
Canopy <- raster("cc2.gri")                          #linear gradient in canopy cover taken from PCA
Precip <- raster("precip.gri")                       #precip

#reformat Precip to same res/extent as elevation
Precip <- resample(x = Precip, y = Elev, "bilinear") #for continuous data
Precip <- mask(Precip, Elev)

#convert Precip to meters
Precip <- Precip / 100
layers <- stack(Canopy, Elev, Precip)          
names(layers) <- c("canopy", "elev", "precip")

#plot
plot(layers)

#inspect
projection(layers)

#label the CRS
crs.layers <- CRS("+proj=aea +lat_1=46 +lat_2=48 +lat_0=44 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
projection(layers) <- crs.layers

#-------------#
#bird data
#-------------#

birds <- read.csv("birdcommunity.csv")

#inspect
head(birds, 3)

#create a spatial points dataframe for converting coordinates
birds.coords <- data.frame(x = birds$LONG_WGS84, y = birds$LAT_WGS84)
birds.attributes <- data.frame(transect = birds$TRANSECT, point = birds$STOP, species = birds$SPECIES, pres = birds$PRES)

#define CRS
crs.latlong <-  CRS("+proj=longlat +datum=WGS84")
birds.spdf <- SpatialPointsDataFrame(birds.coords, data = birds.attributes, proj4string = crs.latlong)

#transform CRS to layers CRS
birds.spdf <- spTransform(birds.spdf, crs.layers)

#new bird data frame
birds.df <- data.frame(birds.spdf@data, x = coordinates(birds.spdf)[,1], y = coordinates(birds.spdf)[,2])

#inspect
names(birds.df)

#plot
plot(Elev)
points(birds.spdf)

#-------------#
#site data
#-------------#

#make a site-species data.frame
species.site <- dcast(birds.df, transect + point + x + y ~ species, value.var = "pres")

#site X species matrix
spp.matrix <- subset(species.site, select = -c(transect, point, x, y))

#subset based on frequency of occurrence
prevalence <- colSums(spp.matrix)
prevalence.20 <- prevalence[prevalence > 20]
species.20 <- names(prevalence.20)
species.matrix <- spp.matrix[,colnames(spp.matrix) %in% species.20]

#extract covariates for sites
site.cov <- extract(layers, species.site[,c("x", "y")])

#inspect
head(site.cov)

#correlation
round(cor(site.cov),2)

######################################################################
# 11.3.3.1 Predict first, assemble later
######################################################################

#Create lists to save the species models
pred.map <- list()                         #stores map predictions
pred.coef <- list()                        #stores coefficients
Nspecies <- ncol(species.matrix)
Nsites <- nrow(species.matrix)

#create vectors for simple processing
canopy <- site.cov[,"canopy"]
elev <- site.cov[,"elev"]
precip <- site.cov[,"precip"]

#Run GLM model for each species
for (i in 1:Nspecies){
  species.i <- glm(species.matrix[,i] ~ canopy + poly(elev,2) + precip, family = binomial)

  #coefficients from model
  pred.coef[[i]] <- coef(species.i)

  #predictions for mapping
  logit.pred <- predict(model = species.i, object = layers, fun = predict)   #prediction on link scale
  prob.pred <- exp(logit.pred) / (1 + exp(logit.pred))                       #back-transform to probability scale
  pred.map[[i]] <- prob.pred
  print(i)
}

prob.map.stack <- stack(pred.map)
names(prob.map.stack) <- colnames(species.matrix)

#Plot of predicted distribution for AMRO
plot(prob.map.stack$AMRO, xlab = "Longitude", ylab = "Latitude",
     main="Predicted map for AMRO - Predict first, assemble later")

#---------------------------------------#
#Create prediction for species richness
#---------------------------------------#

#binary realizations of probabilities
binary.map <- function(map){
  values.i <- values(map)
  binom.i <- rbinom(length(values.i), prob = values.i, size = 1)
  map <- setValues(map,binom.i)
  return(map)
}

#function applies to each layer separately
binary.map.stack <- binary.map(prob.map.stack)

#sum up binary realizations for species richness
spp.binomial.map <- sum(binary.map.stack)

#plot
plot(spp.binomial.map)

#richness from 19 total random deviates
for (i in 1:18){
  binary.map.i <- binary.map(prob.map.stack)
  richness.i <- sum(binary.map.i)
  spp.binomial.map <- addLayer(spp.binomial.map, richness.i)
  print(i)
}

#mean and median predicted richness
spp.mean.map <- mean(spp.binomial.map, na.rm = T)

#plot
plot(spp.mean.map)

#with 19 maps, min and max would approximate 95% CI
spp.lcl.map <- min(spp.binomial.map, na.rm = T)
spp.ucl.map <- max(spp.binomial.map, na.rm = T)

#use threshold instead of binomial
spp.thres.map <- pred.map
for (i in 1:Nspecies){
  thresh.i <- sum(species.matrix[,i]) / Nsites

  spp.thres.map[[i]][which(spp.thres.map[[i]][] > thresh.i)] <- 1
  spp.thres.map[[i]][which(spp.thres.map[[i]][] <= thresh.i)] <- 0
}

# make a multilayered file for species distributions
spp.thres.map <- stack(spp.thres.map)

## Computing species richness
spp.thres.map <- sum(spp.thres.map)

#plot
plot(spp.thres.map)

###########################################################################
# 11.3.3.2 Assemble first, and predict later
###########################################################################

#---------------------------------------#
#species richness modeling
#---------------------------------------#

#calculate species richness
richness <- rowSums(species.matrix)

#Poisson glm
pois.rich <- glm(richness ~ canopy + poly(elev,2) + precip, family = poisson)

#inspect
summary(pois.rich)

#overdispersion test
dispersiontest(pois.rich, trafo = 1)

#map the Poisson model
pois.raster <- predict(model = pois.rich, object = layers, fun = predict) #predict on link scale
spp.raster <- exp(pois.raster)                                            #back transform to count scale

plot(spp.raster, xlab = "Longitude", ylab = "Latitude",
     main="Predicted species richness - Assemble first, predict later")

#make a map of differences between models of richness
spp.diff <- spp.mean.map - spp.raster
plot(spp.diff, xlab = "Longitude", ylab = "Latitude", main="Difference in richness models")

#Raster stack of different species richness predictions
richness.stack <- stack(spp.mean.map, spp.thres.map, spp.raster)
names(richness.stack) <- c("binom-rich", "thres-rich", "pois-rich")

#correlation among richness predictions
richness.map.corr <- layerStats(richness.stack, 'pearson', na.rm = T)

#inspect
richness.map.corr

#remove big files no longer needed
rm(pred.map)
rm(spp.thres.map)

###########################################################################
# 11.3.3.2 Assemble first, and predict later
###########################################################################

#-----------------------------------#
#Generalized dissimilarity modeling
#-----------------------------------#

#format data for gdm
siteID <- 1:nrow(species.matrix)
site.xy <- data.frame(x = species.site$x, y = species.site$y)

gdm.species.matrix <- data.frame(cbind(siteID, site.xy, species.matrix))
gdm.site.matrix <- data.frame(cbind(siteID, site.cov))

#inspect
head(gdm.site.matrix)
head(gdm.species.matrix)

#get gdm formatted object
gdm.data <- formatsitepair(gdm.species.matrix, bioFormat = 1, dist = "bray", abundance = F,
                         XColumn = "x", YColumn = "y", siteColumn = "siteID", predData = gdm.site.matrix)

#run gdm model
gdm.dist <- gdm(gdm.data, geo = T)

#inspect
summary(gdm.dist)
str(gdm.dist)

#contrast gdm without including distance
gdm.nodist <- gdm(gdm.data, geo = F)

#inspect
summary(gdm.nodist)

#partial plots
plot(gdm.dist)

#predictions onto dataframe
gdm.fit <- predict(gdm.dist, gdm.data)

#plot
plot(gdm.data$distance, gdm.fit, xlab="Observed dissimilarity",
     ylab="Predicted dissimilarity",xlim = c(0,1), ylim = c(0,1), lines(c(0,1), c(0,1)))

#predictions for mapping; layers must be in same order as gdm model
gdm.trans.data <- gdm.transform(gdm.dist, layers)

#inspect
class(gdm.trans.data)

#summary stats
cellStats(gdm.trans.data, max)
cellStats(gdm.trans.data, min)

#plot
plot(gdm.trans.data)

#map biological patterns
sample.trans <- sampleRandom(gdm.trans.data, 10000)
sample.pca <- prcomp(sample.trans)

#inspect
summary(sample.pca)

#predict PC scores; note the use of the 'index' argument
gdm.pca <- predict(gdm.trans.data, sample.pca, index = 1:3)

#inspect
summary(gdm.pca)

#plot
plot(gdm.pca)

#scale to 0-1 range; this implicitly makes each the same weeight
gdm.pca <- (gdm.pca - minValue(gdm.pca)) / (maxValue(gdm.pca) - minValue(gdm.pca))

#plot
plot(gdm.pca)

#plot with RGB, adding each layer
plotRGB(gdm.pca,r=1, g=2, b=3, scale=1)

###########################################################################
# 11.3.3.3 Assemble and Predict Together
###########################################################################

#---------------------------#
#cca/rda
#---------------------------#

#vegan rda
rda.bird <- rda(species.matrix ~ canopy + poly(elev,2) + precip)

#inspect
rda.bird
head(summary(rda.bird),2)
coef(rda.bird)
RsquareAdj(rda.bird)$adj.r.squared
scores(rda.bird, choices=1:2, display="sites")
scores(rda.bird, choices=1:2, display="species")

#plot
plot(rda.bird)

#anova-like permutation tests
anova(rda.bird)               #overall test
anova(rda.bird, by = 'mar')   #Type III
anova(rda.bird, by = 'term')  #sequential tests

#map site scores
layers.df <- as.data.frame(layers, xy = T, na.rm = T)

#inspect
head(layers.df)
dim(layers.df)

#predict site scores
rda.site.pred <- predict(rda.bird, layers.df, type = "lc")             #site scores (the linear covariates)

#predict species scores
rda.species.pred <- predict(rda.bird, layers.df, type = "response")    #species scores (the linear covariates)

#inspect
str(rda.site.pred)                                                     #rda axes
str(rda.species.pred)                                                  #species-specific predictions
class(rda.species.pred)

#lc
rda.site.pred.df <- data.frame(x = layers.df$x, y = layers.df$y,
                          RDA1 = rda.site.pred[,1], RDA2 = rda.site.pred[,2])

#species responses
rda.species.pred.df <- data.frame(layers.df, rda.species.pred)

#inspect
head(rda.species.pred.df)
head(rda.site.pred.df)
dim(rda.species.pred.df)
dim(rda.site.pred.df)

#create rasters
rda.vath.map <- rasterFromXYZ(xyz = rda.species.pred.df[,c("x","y","VATH")], res = res(layers), crs = crs.layers)

#plot
plot(rda.vath.map)

###########################################################################
# C. Assemble and predict together
# modeling with multivariate logistic regression
# see Ovaskainen et al. 2010; Ecology
###########################################################################

#merge species and covariates for reshaping the data
species.matrix.df <- data.frame(cbind(site.cov, species.matrix))

#inspect
dim(species.matrix.df)
head(species.matrix.df)
names(species.matrix.df)

#reformate to long format
sp.multi <- melt(species.matrix.df, id.vars = c("elev", "canopy", "precip"),
               variable.name = "SPECIES", value.name = "pres")

#inspect
head(sp.multi)

#random rand intercept model by species
multi.int <- glmer(pres ~ canopy + elev + I(elev^2) + precip + (1|SPECIES), 
                   family = "binomial", data = sp.multi, glmerControl(optimizer = "bobyqa"))

#inspect
summary(multi.int)

#random coefficient model by species: 134 s
multi.coef <- glmer(pres ~ elev + I(elev^2) + canopy + precip + (1|SPECIES)+
                   (0 + elev|SPECIES) + (0 + I(elev^2)|SPECIES) + (0 + canopy|SPECIES) + (0 + precip|SPECIES),
                    family = "binomial", data = sp.multi, glmerControl(optimizer = "bobyqa"))

#inspect
summary(multi.coef)
fixef(multi.coef)
ranef(multi.coef)
coef(multi.coef)
coef(multi.coef)$SPECIES[,"elev"]

#compare to S-SDM
coef(multi.coef)$SPECIES[2,]
pred.coef[[2]]

#-----------------------------#
#Create raster predictions
#-----------------------------#

glmm.map <- list()

# extract maps for all species
for (i in 1:Nspecies){
  logit.raster <- coef(multi.coef)$SPECIES[i,1] +
    coef(multi.coef)$SPECIES[i, "elev"] * Elev +
    coef(multi.coef)$SPECIES[i, "I(elev^2)"] * Elev^2 +
    coef(multi.coef)$SPECIES[i, "canopy"] * Canopy +
    coef(multi.coef)$SPECIES[i, "precip"] * Precip
  prob.raster <- exp(logit.raster) / (1 + exp(logit.raster))
  glmm.map[[i]] <- prob.raster
}

#coefficients from the two models
coef(multi.coef)$SPECIES[2,]        #GLMM coefficients
pred.coef[[2]]                      #GLM coefficients
pred.coef(multi.coef)$SPECIES[2,]

#richness from 19 realizations (add 18 to original):
glmm.map.binomial <- list()
glmm.map.stack <- stack(glmm.map)                     #make raster stack

glmm.binary.map.stack <- binary.map(glmm.map.stack)   #one realization
spp.glmm.binomial.map <- sum(glmm.binary.map.stack)   #richness-one realization

#richness from 19 summaries (add 18 to original):
for (i in 1:18){
  binary.map.i <- binary.map(glmm.map.stack)
  richness.i <- sum(binary.map.i)
  spp.glmm.binomial.map <- addLayer(spp.glmm.binomial.map, richness.i)
  print(i)
}

spp.glmm.mean.map <- mean(spp.glmm.binomial.map)
plot(spp.glmm.mean.map)

#-------------------------------------------#
#plot partial relationships for elevation
#-------------------------------------------#

site.cov.df <- data.frame(site.cov)
precip.mean <- mean(site.cov.df$precip)
canopy.mean <- mean(site.cov.df$canopy)
elev.range <- seq(min(site.cov.df$elev), max(site.cov.df$elev), length = 20)

#create data frame for plotting
newdata.elev.glmm <- data.frame(expand.grid(SPECIES = species.20, precip = precip.mean, elev = elev.range, canopy = canopy.mean))

#predict onto new data frame
pred.elev <- predict(multi.coef, newdata.elev.glmm, type = "response")
glmm.elev.pred <- cbind(newdata.elev.glmm, pred.elev)

###########################################################################
# 11.3.4 Spatial dependence in communities
###########################################################################

#Mantel test for global evidence of autocorrelation

#get distance and dissimilarity matrices
dist.matrix <- as.matrix(dist(site.xy[,c("x", "y")]))
sorenson <- vegdist(species.matrix, method="bray")

#Mantel test
mantel(sorenson, dist.matrix, method = "pearson", permutations = 999)

#---------------------------#
#multivariate correlograms
#---------------------------#

mantel.corr <- mantel.correlog(sorenson, XY = site.xy[,c("x","y")],
                             cutoff = T, r.type = "pearson", nperm = 99)

#plot
plot(mantel.corr)

###########################################################################
# 11.3.5 Community models with explicit accounting for space
###########################################################################

#---------------------------#
#partial rda on distance
#---------------------------#

pcnm.dist <- pcnm(dist.matrix)

#inspect
str(pcnm.dist)
dim(pcnm.dist$vectors)
pcnm.dist$vectors[,2]

#partial rda
rda.partial <- rda(species.matrix ~ canopy + poly(elev,2) + precip + 
                     Condition(scores(pcnm.dist, choices = 1:10)))

#compare
rda.bird
rda.partial

#compare anova-like permuation tests
anova (rda.bird, by = 'mar')
anova (rda.partial, by = 'mar')

#more formal screening of pcnm variables
rda.distance <- rda(species.matrix ~ (scores(pcnm.dist)))  #must use this format for distance matrix

#inspect
rda.distance

#----------------------------------#
#multi-scale ordination
#----------------------------------#

mso.rda <- mso(rda.bird, site.xy, grain = 3000, perm = 19)

#inspect
mso.rda
mso.rda$vario

#plot
plot(mso.rda$vario$H, mso.rda$vario$CA)   #variogram of residual variance
