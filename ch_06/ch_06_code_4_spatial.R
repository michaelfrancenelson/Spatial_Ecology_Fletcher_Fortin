
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




## ---- plotting_params ----
{
  # Thrush data histogram params
  hist_col = "black"
  hist_fill = "steelblue"
  hist_fill_2 = rgb(0, 0.6, 0.9)
  hist_alpha = 0.2
  
  elev_tmp = c(terrain_brick[, , , 1]$elevation)
  rng = range(elev_tmp[elev_tmp > 0], na.rm = T)
  
  gm_hist_1 = 
    geom_histogram(binwidth = 1, color = hist_col, fill = hist_fill, alpha = hist_alpha)
  gm_thrush_hist_1 =
    geom_histogram(bins = 30, color = hist_col, fill = hist_fill, alpha = hist_alpha)
  gm_thrush_hist_2 =
    geom_histogram(bins = 30, color = hist_col, fill = hist_fill_2, alpha = hist_alpha)
  gm_thrush_hist_3 =
    geom_histogram(bins = 18, color = hist_col, fill = hist_fill_2, alpha = hist_alpha)
  
  # Map color and legend params
  legend_width = 0.4
  
  map_cb =
    guide_colorbar(
      title.position = "left",
      barwidth = unit(legend_width, "npc"))
  
  gg_thrush_start = 0.1
  
  gg_thrush_elevation = 
    scale_fill_gradientn(
      colours = gray.colors(3, start = gg_thrush_start),
      na.value = "transparent",
      guide = map_cb,
      limits = c(rng[1], rng[2])
    )
  
  gg_map_vir = 
    scale_fill_viridis(
      na.value = "transparent",
      guide = map_cb)
  
  gg_map_grayscale = 
    scale_fill_gradientn(
      colours = gray.colors(3),
      na.value = "transparent",
      guide = map_cb)
  
  gg_map_terr = 
    scale_fill_gradientn(
      colours = terrain.colors(20),
      na.value = "transparent",
      guide = map_cb)
  
  gg_map_heat = 
    scale_fill_gradientn(
      colours = heat.colors(3),
      na.value = "transparent",
      guide = map_cb)
  
  gg_map = ggplot() +
    coord_equal() +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    theme_map() +
    theme(
      legend.position = "bottom", 
      legend.direction = "horizontal",
      axis.title = element_blank())
  
  rm(rng, elev_tmp)
}






## ---- center_data ----
{
  thrush_sf$elevation_sc = scale(thrush_sf$elevation, center = T, scale = T)
  thrush_sf$slope_sc = scale(thrush_sf$slope, center = T, scale = T)
  thrush_sf$aspect_sc = scale(thrush_sf$aspect, center = T, scale = T)
}



## ---- model_1 ----
{
  fit_elev = glm(
    VATH ~ elevation, 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_elev)
}


## ---- model_2 ----
{
  fit_terrain = glm(
    VATH ~ elevation + slope + aspect, 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_terrain)
}



## ---- model_3 ----
{
  fit_elev_poly =  glm(
    VATH ~ elevation + I(elevation^2), 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_elev_poly)
}

## ---- model_1_center ----
{
  fit_elev_sc = glm(
    VATH ~ elevation_sc, 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_elev_sc)
}


## ---- model_2_center ----
{
  fit_terrain_sc = glm(
    VATH ~ elevation_sc + slope_sc + aspect_sc, 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_terrain_sc)
}



## ---- model_3_center ----
{
  fit_elev_poly_sc =  glm(
    VATH ~ elevation_sc + I(elevation_sc^2), 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_elev_poly_sc)
}



# ---- model_selection ----
{
  AIC(fit_elev_sc, fit_terrain_sc, fit_elev_poly_sc)
}





# ---- spaced_samples_model ----
{
  # Choose one sample point per transect:
  # Use data.table magic to select a random point from each transect
  dat = data.table(thrush_sf)
  dat_2 = thrush_sf[dat[, SURVEYID[sample(length(TRANSECT), 1)], by = TRANSECT]$V1, ]
  
  fit_spaced = 
    glm(
      VATH ~ elevation_sc + I(elevation_sc^2), 
      family = "binomial", 
      data = dat_2
    )
}


# ---- correlogram_response ----
{
  cgram_elev_1 =
    ff_correlogram(
      st_coordinates(thrush_sf),
      thrush_sf$elevation)
}



# ---- correlogram_quadratic ----
{
  cgram_resid_1 =
    ff_correlogram(
      st_coordinates(thrush_sf),
      residuals(fit_elev_poly_sc)
    )
}



# ---- correlogram_spaced ----
{
  
  cgram_resid_spatial =
    ff_correlogram(
      st_coordinates(dat_2),
      residuals(fit_spaced)
    )
}
