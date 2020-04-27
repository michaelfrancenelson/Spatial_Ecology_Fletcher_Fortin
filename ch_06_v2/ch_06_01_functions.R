
# ---- correlogram_function ----
{
  ff_correlogram = function(point_coords, point_data, max_dist = NULL, bin_size = NULL, alpha = 0.05)
  {
    # package 'spdep' needed for dnearneigh() function.
    # Check whether it is installed:
    stopifnot(require(spdep))
    stopifnot(require(raster))
    
    if (is.null(max_dist))
    {
      # Euclidean distance betwen opposite corners of bounding box
      ext_coords = unlist(raster::extent(SpatialPoints(point_coords)))[1:4]
      max_dist = sqrt(diff(ext_coords[1:2])^2 + diff(ext_coords[3:4])^2) / 3
    }
    
    if (is.null(bin_size)) bin_size = max_dist / 20
    
    # Set up the distance bins
    dist_bins = seq(0, max_dist, by = bin_size)
    n_bins = length(dist_bins) - 1
    
    # container for results
    moran_out = data.frame(
      dist_class = rep(NA, n_bins),
      model_I = NA, model_p = NA,
      null_I_lower = NA, null_I_upper = NA)
    
    for (i in 1:n_bins)
    {
      d_lower = dist_bins[i]; d_upper = dist_bins[i+1]
      dist_class = mean(d_lower, d_upper)
      neigh = 
        spdep::dnearneigh(x = point_coords, d1 = d_lower, d2 = d_upper, longlat = F)
      wts = nb2listw(neighbours = neigh, style = 'B',  zero.policy = TRUE)
      
      #note alternative is for P-value, so only 'significant if positive autocorrelation
      mor.i =
        moran.mc(
          x = point_data, listw = wts, nsim = 200, 
          alternative = "greater", zero.policy = T) 
      
      # null envelope	
      null_env = quantile(mor.i$res, probs = c(alpha / 2, 1 - alpha / 2), na.rm = T)
      moran_out[i, ] = c(dist_class, mor.i$statistic, mor.i$p.value, null_env)
    }
    return(moran_out)
  }
  
  # Test case
  if (FALSE)
  {
    point_coords = st_coordinates(thrush_sf)
    point_data = thrush_sf$elevation
    bin_size = 300
    max_dist = 10000
    corr_1 = ff_correlogram(point_coords, point_data)
    head(corr_1)
    with(corr_1, matplot(x = dist_class, y = cbind(model_I, null_I_lower, null_I_upper), type = "l"))
  }
}



## ---- plotting_function ----
{
  plot_ff_correlogram = function(
    cgram, title, subttl = "", 
    thm = theme_light(), ci_lty = 2)
  {
    require(ggplot2)
    return(
      ggplot(cgram, aes(x = dist)) +
        geom_line(aes(y = Morans.i)) +
        geom_line(aes(y = null.upper), lty = ci_lty) +
        geom_line(aes(y = null.lower), lty = ci_lty) +
        ylab("Moran's I") + xlab("distance") +
        ggtitle(title, subttl) +
        geom_hline(yintercept = 0, lty = 3)
    )
  }
}



# ---- ff_correlogram ----

ff_correlogram <- function(
  point_coords, 
  point_data,
  max_dist = NULL,
  bin_size = NULL,
  n_sims = 100,
  alpha = 0.05,
  wt_style = "B",
  alternative = "greater")
{
  # package 'spdep' needed for dnearneigh() function.
  # Check whether it is installed:
  stopifnot(require(spdep))
  stopifnot(require(raster))
  
  # If no max distance is provided, calculate 1/3 of the maximum
  # distance between corners of the bounding box
  if (is.null(max_dist))
  {
    # Euclidean distance betwen opposite corners of bounding box
    ext_coords = unlist(raster::extent(SpatialPoints(point_coords)))[1:4]
    max_dist = sqrt(diff(ext_coords[1:2])^2 + diff(ext_coords[3:4])^2) / 3
  }
  
  # If no bin size is provided, create distance classes for 20 bins:
  if (is.null(bin_size)) bin_size = max_dist / 20
  
  # Set up the distance bins
  dist_bins <- seq(0, max_dist, by = bin_size)
  n_bins <- length(dist_bins) - 1
  
  # container for results
  moran_out <- data.frame(
    dist_class =  rep(NA, n_bins),
    model_I = NA,
    model_p = NA,
    null_I_lower = NA, 
    null_I_upper = NA)
  
  for (i in 1:n_bins)
  {
    d_lower <- dist_bins[i] 
    d_upper <- dist_bins[i+1]
    dist_class = mean(d_lower, d_upper)
    
    neigh = 
      spdep::dnearneigh(
        x = point_coords,
        d1 = d_lower,
        d2 = d_upper, 
        longlat = F)
    
    wts = 
      nb2listw(
        neighbours = neigh, 
        style = wt_style, 
        zero.policy = T)
    
    # note alternative is for P-value, 
    # so only 'significant if positive autocorrelation
    mor.i = 
      moran.mc(
        x = point_data, 
        listw = wts, 
        nsim = n_sims, 
        alternative = alternative, 
        zero.policy = T) 
    
    mor.i$data.name
    
    # null envelope	
    null_env = quantile(
      mor.i$res, 
      probs = c(alpha / 2, 1 - alpha / 2), 
      na.rm = T)
    
    moran_out[i, ] = 
      c(
        dist_class = dist_class,
        model_I = mor.i$statistic,
        model_p = mor.i$p.value,
        null_env
      )
  }
  return(moran_out)
}


# ---- plot_ff_correlogram ----
plot_ff_correlogram = function(
  corr_1,
  x_lab = "Distance Class",
  y_lab = "Moran's I",
  title = " ")
{
  dat_env = with(
    corr_1, 
    data.frame(
      x = c(dist_class, rev(dist_class)), 
      y = c(null_I_lower, rev(null_I_upper))))
  
  ggplot(corr_1, aes(x = dist_class, y = model_I)) +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2) +
    geom_line(aes(y = null_I_lower), col = gray(0, 0.3)) +
    geom_line(aes(y = null_I_upper), col = gray(0, 0.3)) +
    geom_polygon(aes(x, y), data = dat_env, fill = gray(0, 0.1)) +
    xlab(x_lab) +
    ylab(y_lab) +
    ggtitle(title)
}



# ---- correlogram_test_data ----
point_coords = st_coordinates(thrush_sf)
point_data = thrush_sf$elev
bin_size = 300
max_dist = 10000
alpha = 0.05


# ---- correlogram_test_plot
corr_1 = ff_correlogram(st_coordinates(thrush_sf), thrush_sf$elevation, n_sims = 50)
plot_ff_correlogram(corr_1)



