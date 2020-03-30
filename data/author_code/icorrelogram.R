##########################################################
#CH 6 FUNCTIONS
##########################################################


icorrelogram <- function(locations,z, binsize, maxdist){
  
  distbin <- seq(0,maxdist,by=binsize)
  Nbin <- length(distbin)-1
  moran.results <- data.frame("dist"= rep(NA,Nbin), "Morans.i"=NA,"null.lower"=NA, "null.lower"=NA)
  
  for (i in 1:Nbin){
    d.start<-distbin[i] 
    d.end<-distbin[i+1]
    neigh <- dnearneigh(x=locations, d1=d.start, d.end, longlat=F)
    wts <- nb2listw(neighbours=neigh, style='B', zero.policy=T)
    mor.i <- moran.mc(x=z, listw=wts, nsim=200, alternative="greater", zero.policy=T)  #note alternative is for P-value, so only 'significant if positive autocorrelation
    
    moran.results[i, "dist"]<-(d.end+d.start)/2 
    moran.results[i, "Morans.i"]<-mor.i$statistic 								                #observed moran's i
    moran.results[i, "null.lower"]<-quantile(mor.i$res, probs = 0.025,na.rm = T)#95% null envelope	
    moran.results[i, "null.upper"]<-quantile(mor.i$res, probs = 0.975,na.rm = T)#95% null envelope
  }
  return(moran.results)
}