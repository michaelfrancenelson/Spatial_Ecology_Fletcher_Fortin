########################################################
########################################################
#Fletcher and Fortin 2019
#Chapter 9: Connectivity
########################################################
########################################################

#load packages
library(raster)           #for raster covariate data; version 2.6-7 used
library(rgdal)            #for reading different types of GIS files; version 1.3-4 used
library(rgeos)            #for centroids of polygons; version 0.3-28 used
library(gdistance)        #for least-cost paths/circuit theory; version 1.2-2 used
library(igraph)           #for patch-based graphs; version 1.2.2 used
library(fitdistrplus)     #for kernels; version 1.0-11 used
library(fdrtool)          #for 1Dt kernel; version 1.2.15 used

#set working directory where data were downloaded
setwd(choose.dir())

#increase memory
mem.max <- memory.limit(size=NA)
memory.limit(size=mem.max)

######################################################
#9.3.3 Florida panthers
######################################################

land <- raster("panther_landcover")

#inspect
projection(land)
res(land)

#label projection for later use
crs.land <- projection(land)

#public areas in need of connections
public <- readOGR("panther_publicland.shp")
projection(public)
projection(public) <- crs.land
names(public@data)                        #attributes table
head(public@data)                         #attributes table

#get the centroids of plots
public_centroids <- gCentroid(public, byid=T)
public_centroids@coords
public_centroids@coords[1,]               #x,y of first site

#------------------------------------#
#create resistance map
#------------------------------------#

#import reclassification table
classification <- read.table("resistance reclass.txt", header=T)

#inspect
head(classification,3)

#reclass
class <- as.matrix(classification[,c(1,3)])
land_cost <- reclassify(land,rcl=class)

#plot
plot(land_cost)
plot(public, add=T)
points(public_centroids, col="grey30")

###########################################################
#9.3.3.1 Effective distances
###########################################################

#create a conductance transition layer: inverse of resistance data
land_cond <- transition(1/land_cost, transitionFunction=mean, 8)

#make correction; type=c for lcps; type = r for circuit (identical results for this example, so just use c)
land_cond <- geoCorrection(land_cond, type="c", multpl=F)

#geographic (Euclidean) distance matrix
geo.dist <- pointDistance(public_centroids, lonlat=FALSE)
geo.dist <- as.dist(geo.dist)

#least-cost distance matrix (0.68 sec on my computer)
lc.dist <- costDistance(land_cond, public_centroids)

#commutedistance matrix (422 sec on my computer)
circuit.dist <- commuteDistance(land_cond, public_centroids)

#randomized shortest-paths distance matrix (~748 sec on my computer)
rSP.dist_t0001 <- rSPDistance(land_cond, from=public_centroids, to=public_centroids, theta=0.0001)

#inspect
geo.dist
lc.dist
circuit.dist
rSP.dist_t0001

#take lower triangle of rSP.dist_t0001
rSP.dist_t0001.tri <- rSP.dist_t0001
rSP.dist_t0001.tri[upper.tri(rSP.dist_t0001.tri, diag=TRUE)] <- NA

#inspect
rSP.dist_t0001.tri

#make data frame of distances
all.dist <- data.frame(Euclidean=as.vector(geo.dist),
                     lcd=as.vector(lc.dist),
                     circuit=as.vector(circuit.dist),
                     rSP=na.omit(as.vector(rSP.dist_t0001.tri)))

#correlation
round(cor(all.dist),3)

###################################################
#9.3.3.2 Least-cost paths
###################################################

#attribute table
public@data

#crop to focal area
fpwr_ossf_extent <- extent(642000,683000,237000,298000)
land_sub <- crop(land, fpwr_ossf_extent)
land_cost_sub <- crop(land_cost, fpwr_ossf_extent)
land_cond_sub <- transition(1/land_cost_sub, transitionFunction=mean, 8)
land_cond_sub <- geoCorrection(land_cond_sub, type="c", multpl=FALSE)

#get lcp
fpwr_ossf_lcp <- shortestPath(land_cond, public_centroids@coords[5,], public_centroids@coords[3,], output="SpatialLines")

#plot
plot(land_cost_sub, axes=F, box=F)
plot(public, add=T)
points(public_centroids, col="grey20")
lines(fpwr_ossf_lcp, col="red", lw=3)

############################################
#9.3.3.3 Least-cost corridor
############################################

#get cumulative costs from each PA
fpwr.cost <- accCost(land_cond_sub, public_centroids@coords[5,])
ossf.cost <- accCost(land_cond_sub, public_centroids@coords[3,])

#plot
par(mfrow=c(1,2))
plot(fpwr.cost)
plot(ossf.cost)
dev.off()

#get least-cost corridor
leastcost_corridor <- overlay(fpwr.cost, ossf.cost, fun=function(x, y){return(x + y)})

#plot
plot(leastcost_corridor, legend=F, axes=F)
plot(public, add=T)
points(public_centroids, col="grey30")

#get lower quantile
quantile10 <- quantile(leastcost_corridor, probs=0.10, na.rm=TRUE)

#make new truncated layer
leastcost_corridor10 <- leastcost_corridor
values(leastcost_corridor10) <- NA
leastcost_corridor10[leastcost_corridor < quantile10] <- 1 #truncate to identify corridor

#plot
plot(leastcost_corridor, legend=F, axes=F)
plot(leastcost_corridor10, legend=F,axes=F, add=T)
points(public_centroids, col="grey30")
lines(fpwr_ossf_lcp, col="red", lw=3)

#------------------------------------#
#Relating paths to land-cover types
#------------------------------------#

#identify land-cover along the lcp
lcp.land <- extract(land, fpwr_ossf_lcp)

#summarize
table(lcp.land)

#identify land-cover along the least-cost corridor
corridor.land <- mask(land_sub, leastcost_corridor10)

#summarize
table(as.vector(corridor.land))
classification[,1:2]#cross-walk IDs with descriptions

#plot
plot(corridor.land, axes=F, legend=F)

############################################
#9.3.3.4 Flow mapping
############################################

#flow mapping under different thetas
passage.map_t0 <- passage(land_cond_sub, origin=public_centroids@coords[3,], goal=public_centroids@coords[5,], theta=0)
passage.map_t000001 <- passage(land_cond_sub, origin=public_centroids@coords[3,], goal=public_centroids@coords[5,], theta=0.000001,totalNet = "total")
passage.map_t00001 <- passage(land_cond_sub, origin=public_centroids@coords[3,], goal=public_centroids@coords[5,], theta=0.00001,totalNet = "total")
passage.map_t0001 <- passage(land_cond_sub, origin=public_centroids@coords[3,], goal=public_centroids@coords[5,], theta=0.0001,totalNet = "total")
passage.map_t001 <- passage(land_cond_sub, origin=public_centroids@coords[3,], goal=public_centroids@coords[5,], theta=0.001)
passage.map_t005 <- passage(land_cond_sub, origin=public_centroids@coords[3,], goal=public_centroids@coords[5,], theta=0.005)

#plot
plot(passage.map_t0, axes=F, legend=F)
plot(passage.map_t000001, axes=F, legend=F)
plot(passage.map_t00001, axes=F, legend=F)
plot(passage.map_t0001, axes=F, legend=F)
plot(passage.map_t001, axes=F, legend=F)
plot(passage.map_t005, axes=F, legend=F)

###############################################################
# 9.3.4 Patch-based graphs and network connectivity
###############################################################

#site attributes
nodes <- read.csv("kite_nodes.csv", header=T)

#inspect
head(nodes)

#make vector of patch area for later use
area <- nodes$area #in km^2

#movement data of snail kites
A.obs <- read.csv("kite_movement.csv", header=T)
A.obs <- as.matrix(A.obs[,2:30])

#inspect
dim(A.obs)

#re-label
rownames(A.obs) <- 1:29
colnames(A.obs) <- 1:29
diag(A.obs) <- 0

#inspect
View(A.obs)

#make distance matrix
coords <- cbind(nodes$XCoord, nodes$YCoord)
distmat <- pointDistance(coords, lonlat=F)
distmat <- distmat/1000 #in km

####################################
#9.3.4.1 Dispersal kernels
####################################

link.loc <- which(A.obs>0, arr.ind=T)

#inspect
link.loc

#link movements with distances between sites
within_disp <- cbind(distmat[link.loc], A.obs[link.loc])

#inspect
head(within_disp)

#repeat distances based on freq of movements for each distance (second column)
within_disp <- rep(within_disp[,1], within_disp[,2])

#plot
hist(within_disp)

#get summary stats for dispersal distances
mean.dist <- mean(within_disp)         #mean is 72
max.dist <- max(within_disp)           #max movement distance is 267 km
max(distmat)                           #max distance between nodes: 296 km

#fit kernels to the data
disp.lnorm <- fitdist(data=within_disp, distr="lnorm", method="mle")
disp.exp <- fitdist(data=within_disp, distr="exp", method="mle")
disp.weib <- fitdist(data=within_disp, distr="weibull", method="mle")
disp.1dt <- fitdist(data=within_disp, distr="halfnorm", start=list(theta=0.01), method="mle")

disp.AIC <- gofstat(list(disp.exp, disp.lnorm, disp.weib, disp.1dt),
                  fitnames=c("exponential", "lognormal", "Weibull", "1Dt"))

#inspect
disp.AIC$aic
summary(disp.exp)
summary(disp.weib)

#plot
plot(disp.weib)
plot(disp.exp)

#################################################
#9.3.4.2 Creating a patch-based graph
#################################################

#Create a binary adjacency matrix with mean distance
A.mean <- matrix(0, nrow=nrow(A.obs), ncol=ncol(A.obs))
A.mean[distmat < mean.dist] <- 1
diag(A.mean) <- 0

#Create adjacency matrix with negative exponential function
A.prob <- matrix(0, nrow=nrow(A.obs), ncol=ncol(A.obs))
alpha <- 1/mean.dist
A.prob <- exp(-alpha*distmat)
diag(A.prob) <- 0

#create igraph objects
graph.Amean <- graph.adjacency(A.mean, mode="undirected", weighted=NULL)
graph.Aprob <- graph.adjacency(A.prob, mode="undirected", weighted=T)
graph.Aobs <- graph.adjacency(A.obs, mode="directed", weighted=T)

#inspect: access graph properties
V(graph.Aobs)
head(E(graph.Aobs))
head(E(graph.Aobs)$weight)

#plot
plot(graph.Amean, layout=coords)
plot(graph.Aprob, layout=coords, edge.width=E(graph.Aprob)$weight*4, vertex.label=NA)
plot(graph.Aobs, layout=coords, edge.width=E(graph.Aobs)$weight/2, vertex.label=NA)

###############################################
#9.3.4.3 Patch connectivity
###############################################

Amean.degree <- degree(graph.Amean)
Amean.eigen <- evcent(graph.Amean)
Amean.close <- closeness(graph.Amean)
Amean.between <- betweenness(graph.Amean)

cor(cbind(Amean.degree, Amean.eigen$vector, Amean.close, Amean.between))

#betweennes centrality for weighted graph
Aprob.between <- betweenness(graph.Aprob, weights=1/E(graph.Aprob)$weight)

#plot
par(mfrow=c(1,3))
plot(graph.Amean, layout=coords,vertex.size=2*(Amean.degree)^0.5, main= "Degree",
    vertex.label.dist=0.5,vertex.label.color="transparent", vertex.label.cex=0.5, frame=T)
plot(graph.Amean, layout=coords,vertex.size=2*(Amean.eigen$vector)*8, main= "Eigenvector",
    vertex.label.color="transparent", vertex.label.cex=0.5, frame=T)
plot(graph.Amean, layout=coords,vertex.size=2*(Amean.betweenness)^0.5, main= "Betweenness",
     vertex.label.color="transparent", vertex.label.cex=0.5, frame=T)
dev.off()

############################################
#9.3.4.4 meso-scale connectivity
############################################

#components/clusters
Amean.Ncomponents <- clusters(graph.Amean)$no
Amean.Membcomponents <- clusters(graph.Amean)$membership

#inspect
Amean.Ncomponents
Amean.Membcomponents

#modularity
Amean.modularity <- cluster_louvain(graph.Amean)

#inspect
modularity(Amean.modularity)
membership(Amean.modularity)

#plot
par(mfrow=c(1,2))
plot(graph.Amean,layout=coords,vertex.size=8, main= "Components",
     vertex.label.color="transparent", vertex.color=Amean.Membcomponents, vertex.label.cex=0.5, frame=T)
plot(graph.Amean,layout=coords,vertex.size=8, main= "Modularity",
     vertex.label.color="transparent", vertex.color=Amean.modularity$membership, vertex.label.cex=0.5, frame=T)
dev.off()

#######################################
#9.3.4.5 landscape-scale connectivity
#######################################

#Landscape coincidence probability, LCP
coords <- data.frame(x=coords[,1], y=coords[,2])
AL <- 63990                                                                   #area taken from study area polygon (not shown)
components.area <- tapply(area, clusters(graph.Amean)$membership, sum)
LCP <- sum((components.area/AL)^2)

#connectance
connectance <- graph.density(graph.Amean)

#IIC
IIC <- function(Amatrix, area,landarea){

  A.graph <- graph.adjacency(Amatrix, mode="undirected", weighted=NULL)
  nl.mat <- shortest.paths(A.graph)
  nl.mat[is.infinite(nl.mat)] <- 1000
  IICmat <- outer(area, area)/(1+nl.mat)
  IIC <- sum(IICmat)/landarea^2

  return(IIC)
}

#Probability of Connectivity, PC
pstar.mat <- shortest.paths(graph.Aprob, weights= -log(E(graph.Aprob)$weight)) #calculate p*
pstar.mat <- exp(-pstar.mat)                                                   #back-transform to probabilities; diagonal=1
PCnum <- outer(area, area)*pstar.mat                                           #numerator of PC
PC <- sum(PCnum)/AL^2

#function for PC and its fractions
prob.connectivity <- function(prob.matrix, area,landarea){

  #PC
  pc.graph <- graph.adjacency(prob.matrix, mode="undirected", weighted=TRUE)
  pstar.mat <- shortest.paths(pc.graph, weights= -log(E(pc.graph)$weight))
  pstar.mat <- exp(-pstar.mat)
  PCmat <- outer(area, area)*pstar.mat
  PC <- sum(PCmat)/landarea^2

  #dPC
  N <- nrow(prob.matrix)
  dPC <- rep(NA, N)

  for (i in 1:N) {
    prob.matrix.i <- prob.matrix[-i,-i]
    area.i <-area[-i]
    pc.graph.i <- graph.adjacency(prob.matrix.i, mode="undirected", weighted=TRUE)
    pstar.mat.i <- shortest.paths(pc.graph.i, weights= -log(E(pc.graph.i)$weight))
    pstar.mat.i <- exp(-pstar.mat.i)
    PCmat.i <- outer(area.i, area.i)*pstar.mat.i
    PC.i <- sum(PCmat.i)/landarea^2
    dPC[i] <- (PC-PC.i)/PC*100
  }

  #dPC fractions
  dPCintra <- area^2/sum(PCmat)*100
  dPCflux <- 2*(rowSums(PCmat)-area^2)/sum(PCmat)*100
  dPCconn <- dPC-dPCintra-dPCflux

  #summarize
  patch.metrics <- data.frame(dPC, dPCintra, dPCflux, dPCconn)
  pc.list <- list(PC=PC, patch=patch.metrics)
  return(pc.list)
}

Aprob.PC <- prob.connectivity(prob.matrix=A.prob, area=area, landarea=AL)
Aprob.PC[[1]]
Aprob.PC[[2]]
round(Aprob.PC[[2]]$dPC,3)


#comparing patch metrics for A.prob
Aprob.strength <- strength(graph.Aprob)
Aprob.eigen <- evcent(graph.Aprob)
Aprob.close <- closeness(graph.Aprob, weights= 1/(E(graph.Aprob)$weight))
Aprob.between <- betweenness(graph.Aprob, weights= 1/(E(graph.Aprob)$weight))

#make data frame
Aprob.centrality <- data.frame(area=area, strength=Aprob.strength,
                             eigen=Aprob.eigen$vector, close=Aprob.close,between=Aprob.between,
                             dPC=Aprob.PC[[2]]$dPC, dPCintra=Aprob.PC[[2]]$dPCintra, dPCflux=Aprob.PC[[2]]$dPCflux, dPCconn=Aprob.PC[[2]]$dPCconn)

#correlation
round(cor(Aprob.centrality),2)

########################################################
#9.3.5 Combining connectivity mapping with graph theory
########################################################

#transition matrix for raster layer
land_cost_subt <- transition(land_cost_sub, transitionFunction=mean, 8)
land_cost_subt <- geoCorrection(land_cost_subt, type="c", multpl=FALSE)
land.matrix <- transitionMatrix(land_cost_subt)                              #sparse matrix of transition layer

#now take the sparse matrix and use igraph for analysis
land.graph <- graph.adjacency(land.matrix, mode="undirected", weighted=T)    #takes sparse matrices from Matrix
land.between <- betweenness(land.graph, directed=F)

#map the betweenness and contrast with least-cost corridor
land.between.map <- setValues(land_cost_sub, land.between)

#plot
par(mfrow=c(1,3))
plot(land_cost_sub, axes=F, legend=F, main="cost layer, LCP")
plot(public, add=T)
lines(fpwr_ossf_lcp, col="red", lw=3)

plot((land.between.map)^0.5, axes=F, legend=F,  main="betweenness")
plot(public, add=T)

plot(corridor.land, legend=F, axes=F, main="Least-cost Corridor")
plot(public, add=T)
dev.off()
