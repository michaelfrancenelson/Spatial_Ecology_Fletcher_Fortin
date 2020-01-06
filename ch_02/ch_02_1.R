if (!("raster" %in% installed.packages()[, 1])) install.packages("raster")
# packages ----
{
  require(raster)
}

# 2.3.3 Simulated example ----
{
  
  set.seed(16)
  
  lambda = 3.0
  
  toy = raster(
    ncol = 6, nrow = 6, 
    xmn = 1, xmx = 6,
    ymn = 1, ymx = 6)
  
  values(toy) = rpois(ncell(toy), lambda = lambda)
  
  
}




