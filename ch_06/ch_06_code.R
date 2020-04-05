source(here::here("/data/environment_vars.R"))

# ---- packages ----
{
  require(here)
  require(data.table)
  
  # simple features packages
  require(stars)
  require(sf)
  
  # original spatial packages
  require(raster)
  require(sp)
  
  # ggplot related packages
  require(ggplot2)
  require(cowplot)
  require(viridis)
  
  # for a nice pair plot function
  require(psych)
  
  # for Moran's I function
  require(spdep)
  
  # CRS for the raster and point data
  crs_dat = "+proj=aea +lat_1=46 +lat_2=48 +lat_0=44 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  
  # Save output images?
  save_images = FALSE
  save_images = TRUE
  execute = TRUE
}



# ---- elevation_data ----
{
  dem = raster(file.path(book_data, "elev"))
  proj4string(dem) = crs_dat
  
  # There are a bunch of zeroes in the data, but there are no regions in the intermountain
  # west with elevaiton of 0.  
  # Replace these with NA
  dem[][dem[] == 0] = NA
  
  dem_m = dem
  dem_m[] = dem[] * 1e3
  
  dat_brick = brick(dem_m, terrain(dem_m, opt = c("slope", "aspect"), unit = "radians"))
  
  names(dat_brick)[1] = "elevation"
  terrain_brick = st_as_stars(dat_brick)
  
  # resamplign to make plotting faster
  r_t = raster(extent(dat_brick), resolution = 5000)
  terrain_brick_rs = st_as_stars(resample(dat_brick, raster(extent(dat_brick), resolution = 5000)))
  # terrain_brick_rs = st_as_stars(resample(dat_brick, raster(extent(dat_brick), resolution = 5000)))
  
  rm(dem_m, dem)
}




#  ---- varied_thrush_data ----
{
  thrush_dt =
    fread(file.path(book_data, "vath_2004.csv"), header = TRUE)
  
  thrush_dt[, present := ifelse(VATH == 0, "absent", "present")]
  thrush_dt[, count := VATH]
  
  thrush_sf = st_as_sf(
    thrush_dt,
    coords = c("EASTING", "NORTHING"),
    crs = crs_dat)
  
  thrush_sf = cbind(thrush_sf, extract(dat_brick, thrush_dt[, .(x = EASTING, y = NORTHING)]))
}




# ---- correlogram_function ----
{
  icorrelogram <- function(locations, z, binsize, maxdist)
  {
    # package 'spdep' needed for dnearneigh() function.
    # Check whether it is installed:
    stopifnot(require(spdep))
    
    distbin <- seq(0, maxdist, by = binsize)
    Nbin <- length(distbin)-1
    
    moran.results <- data.frame(
      "dist" =  rep(NA, Nbin),
      "Morans.i" = NA,
      "null.lower" = NA,
      "null.upper" = NA)
    
    for (i in 1:Nbin){
      d.start <- distbin[i]
      d.end <- distbin[i+1]
      
      neigh <- spdep::dnearneigh(
        x = locations,
        d1 = d.start,
        d.end,
        longlat = F)
      
      wts <- nb2listw(
        neighbours = neigh,
        style = 'B',
        zero.policy = T)
      
      #note alternative is for P-value, so only 'signif icant if positive autocorrelation
      mor.i <- moran.mc(
        x = z, listw = wts,
        nsim = 200,
        alternative = "greater",
        zero.policy = T)
      
      moran.results[i, "dist"] <- (d.end+d.start)/2
      
      #observed moran's i
      moran.results[i, "Morans.i"] <- mor.i$statistic
      
      moran.results[i, "null.lower"] <-
        quantile(
          mor.i$res,
          probs = 0.025,
          na.rm = T)#95% null envelope
      
      moran.results[i, "null.upper"] <-
        quantile(
          mor.i$res,
          probs = 0.975,
          na.rm = T)#95% null envelope
    }
    return(moran.results)
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




## ---- covariate_pair_plot ----
{
  pairs.panels(
    data.frame(thrush_sf)[, 
                          c("elevation", "slope", "aspect")])
}




## ---- census_histograms ----
{
  require(grid)
  require(ggplotify)
  require(ggplot2)
  
  pplot = as.ggplot(
    ~pairs.panels(
      data.frame(thrush_sf)[, c("elevation", "slope", "aspect")]))
  
  g_hist_census = 
    ggplot(
      aggregate(
        VATH ~ TRANSECT, 
        data = thrush_sf, sum),
      aes(x = VATH)) + 
    gm_hist_1 +
    labs(
      title = "Varied Thrush Census", 
      subtitle = "Counts aggregated by transect") +
    xlab("Census Count")
  
  g_hist_census_points = 
    ggplot(
      aggregate(
        VATH ~ TRANSECT * POINT, 
        data = thrush_sf, sum), 
      aes(x = VATH)) + 
    gm_hist_1 +
    labs(
      title = "Varied Thrush Census", 
      subtitle = "Counts aggregated by sample points in transects") +
    xlab("Census Count")
}

## ---- terrain_histograms ----
{
  gm_hist_thrush = ggplot(thrush_sf) + gm_thrush_hist_2
  
  g_hist_elev = 
    gm_hist_thrush + aes(x = elevation) +
    ggtitle("Sample Point Terrain: Elevation") + 
    xlab("Elevation (m)")
  
  g_hist_slope = 
    gm_hist_thrush +  aes(x = slope) +
    ggtitle("Sample Point Terrain: Slope") + 
    xlab("Slope (m / km)")
  
  g_hist_aspect = 
    gm_hist_thrush + aes(x = aspect) + 
    ggtitle("Sample Point Terrain: Aspect") + 
    xlab("Aspect (radians)")
  
  g_hist_terrain = plot_grid(
    g_hist_elev,
    g_hist_slope, 
    g_hist_aspect, ncol = 1)
  
  # Conditional histograms
  gm_hist_thrush_present = 
    ggplot(subset(thrush_sf, VATH == 1)) +
    gm_thrush_hist_3
  gm_hist_thrush_absent = 
    ggplot(subset(thrush_sf, VATH == 0)) +
    gm_thrush_hist_3
  
  g_hist_elev_present = 
    gm_hist_thrush_present + aes(x = elevation) +
    labs(
      title ="Sample Point Elevation",
      subtitle = "Thrushes Present") + 
    xlab("Elevation (m)")
  
  g_hist_elev_absent = 
    gm_hist_thrush_absent + aes(x = elevation) +
    labs(
      title = "Sample Point Elevation", 
      subtitle = "Thrushes Absent") + 
    xlab("Elevation (m)")
  
  g_hist_elev_cond = plot_grid(
    g_hist_elev_present, g_hist_elev_absent, nrow = 1)
  
  
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




## ---- model_diagnostics_1 ----
{
  plot(fit_elev)
  
  fit_elev$predicted = predict(fit_elev, type="response")
  fit_elev$residuals = residuals(fit_elev, type = "response")

  d$predicted <- predict(fit, type="response")
  d$residuals <- residuals(fit, type = "response")

  
  d$predicted <- predict(fit, type="response")
  d$residuals <- residuals(fit, type = "response")
  
  # Steps 3 and 4: plot the results
  ggplot(d, aes(x = hp, y = vs)) +
    geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
  
  # Steps 3 and 4: plot the results
  ggplot(d, aes(x = hp, y = vs)) +
    geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()  
  # Steps 3 and 4: plot the results
  ggplot(d, aes(x = hp, y = vs)) +
    geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
  thrush_sf$predicted = predict(fit_elev, type="response")
  thrush_sf$residuals = residuals(fit_elev, type = "response")
  
    # Steps 3 and 4: plot the results
  ggplot(thrush_sf, aes(x = elevation_sc, y = VATH)) +
    geom_segment(aes(xend = elevation_sc, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
    fit_elev$predicted
  
}

## ---- model_1_diagnostics ----
{
  dat_fit_1 = data.frame(thrush_sf)
  dat_fit_1$predicted = predict(fit_elev_sc, type="response")
  dat_fit_1$residuals = residuals(fit_elev_sc, type="response")
   
  # plot residuals against elevation (the predictor)
  plot(residuals ~ elevation, data = dat_fit_1) 
  
  # Plot residuals against the predicted values
  plot(residuals ~ predicted, data = dat_fit_1) 
}


## ---- model_3_diagnostics ----
{
  dat_fit_3 = data.frame(thrush_sf)
  dat_fit_3$predicted = predict(fit_elev_poly_sc, type="response")
  dat_fit_3$residuals = residuals(fit_elev_poly_sc, type="response")  
  # plot residuals against elevation (the predictor)
  
  plot(residuals ~ elevation, data = dat_fit_3) 
  
  # Plot residuals against the predicted values
  plot(residuals ~ predicted, data = dat_fit_3, pch = 16, cex = 0.1)
} 

## ---- model_1_diagnostics_fancy ----
if (FALSE)
  {
  
  with(subset(dat_fit_1, VATH == 0), lines(lowess(elevation, residuals)))
  with(subset(dat_fit_1, VATH == 1), lines(lowess(elevation, residuals)))
  with(dat_fit_1, lines(lowess(elevation, residuals)))
  
  plot(residuals ~ predicted, data = dat_fit_1, type = ) 
  # with(subset(dat_fit_1, VATH == 0), lines(lowess(predicted, residuals)))
  # with(subset(dat_fit_1, VATH == 1), lines(lowess(predicted, residuals)))
  with(dat_fit_1, lines(lowess(predicted, residuals)))
  
  
  plot(predicted ~ elevation, data = dat_fit_1) 
  ggplot(dat_fit_1, aes(x = elevation, y = residuals)) +
    # geom_segment(aes(xend = elevation, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
  
  
  # Steps 3 and 4: plot the results
  ggplot(dat_fit_1, aes(x = predicted, y = residuals)) +
    # geom_segment(aes(xend = predicted, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    # geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
  
}


 
## ---- model_3_diagnostics_fancy ----
if (FALSE)
  {

  
  ggplot(dat_fit_1, aes(x = elevation, y = VATH)) +
    geom_segment(aes(xend = elevation, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  plot(residuals ~ predicted, data = dat_fit_3)   
}



# ---- model_selection ----
{
  AIC(fit_elev_sc, fit_terrain_sc, fit_elev_poly_sc)
}




# ---- model_plots ----
{
  newdata = data.frame(
    elevation_sc =   
      seq(
        min(thrush_sf$elevation_sc), 
        max(thrush_sf$elevation_sc), 
        length = 25))
  
  
  
  # Use model to predict values for different elevations:
  # type = response for predicted probabilities
  glm.pred = predict(
    fit_elev_sc, 
    newdata = newdata, 
    type =  "link", 
    se = T) 
  
  
}
## ---- image_file_parameters ----
{
  # dimensions for output files
  map_panel_width = 4.5
  map_panel_height = 6
  map_res = 250
  
  fig_panel_width = 6
  fig_panel_height_1 = 2
  
  save_th = function(filename, grob, height = 1, width = 1)
  {
    pdf(file.path(fig_dir, filename), 
        width = width * fig_panel_width, 
        height = height * fig_panel_height_1)
    print(grob)
    dev.off()
    
  }
  
  save_th_map = function(filename, grob, height = 1, width = 1)
  {
    png(file.path(fig_dir, filename), 
        width = width * map_panel_width, 
        height = height * map_panel_height, 
        units = "in", res = map_res)
    print(grob)
    dev.off()
  }
  
}




## ---- terrain_maps ----
{
  # original raster set for hi-resolution maps
  dat_br = terrain_brick
  
  # reduced-resolution raster set to speed up plotting
  dat_br = terrain_brick_rs
  
  # Select which color ramps to use for the maps
  cols = gg_map_grayscale
  cols = gg_map_terr
  
  map_elev = 
    gg_map +
    geom_stars(data = dat_br[, , , 1]) + 
    labs(fill = "Elevation") + cols
  
  map_slope = 
    gg_map +
    geom_stars(data = dat_br[, , , 2]) + 
    labs(fill = "Slope") + cols
  
  map_aspect = 
    gg_map + 
    geom_stars(data = dat_br[, , , 3]) + 
    labs(fill = "Aspect") + cols
  
  map_terrain =
    plot_grid(
      map_elev,
      map_slope, 
      map_aspect,
      nrow = 1)
}



## ---- census_maps ----
{
  bb_inset = st_bbox(
    c(xmin = 170000,
      xmax = 210000,
      ymin = 500000,
      ymax = 551000), 
    crs = crs_dat)
  
  # full resolution
  dat_br = terrain_brick
  
  # To speed  up test plots
  dat_br = terrain_brick_rs
  
  st_crs(terrain_brick_rs) = crs_dat
  
  thrush_inset = st_crop(thrush_sf, bb_inset)
  elev_inset = st_crop(terrain_brick[, , , 1], bb_inset)
  
  thrush_inset = st_crop(thrush_sf, bb_inset)
  elev_inset = st_crop(terrain_brick[, , , 1], bb_inset)
  
  cols = gg_thrush_elevation
  sc_census = scale_color_manual(name = "Varied Thrush", values = c(2, 3))
  
  thrush_inset_poly = 
    geom_sf(data = st_as_sfc(bb_inset),
            fill = "transparent",
            col = rgb(0, 0.3, 0.001), size = 1.9)
  
  thrush_map = 
    gg_map + 
    geom_stars(data = dat_br[, , , 1]) +
    geom_sf(
      aes(colour = present),
      data = thrush_sf,
      size = 3, show.legend = FALSE) +
    labs(
      title = "Sampling Locations",
      subtitle = "Transects",
      fill = "Elevation") + cols + sc_census
  
  thrush_inset_map = 
    gg_map + 
    geom_stars(data = elev_inset, show.legend = FALSE) +
    geom_sf(
      aes(colour = present),
      data = thrush_inset, 
      size = 2, 
      show.legend = "point") +
    labs(
      title = "Sampling Locations", 
      subtitle = "Census Sites",
      fill = "Elevation") + 
    cols + sc_census +
    guides(
      colour = guide_legend(title.position="top", title.hjust = 0.5))
  # theme(legend.title = element_blank())
  
  thrush_map_full = plot_grid(
    thrush_map + thrush_inset_poly, 
    thrush_inset_map, nrow = 1)
  
}




## ---- save_census_maps ----
{
  if (save_images)
  {
    save_th_map("varied_thrush_inset.png", thrush_inset_map)
    save_th_map("varied_thrush_map.png", thrush_map)
    save_th_map("varied_thrush_map_and_inset.png", thrush_map_full, width = 2)
  }
}


## ---- save_figures ----
{
  if (save_images)
  {
    
    pdf(file.path(fig_dir, "varied_thrush_pair_plot.pdf"), 
        width = fig_panel_width, height = 2 * fig_panel_height_1)
    print(pairs.panels(data.frame(thrush_sf)[, c("elevation", "slope", "aspect")]))
    dev.off()
    
    save_th("varied_thrush_census_hist_transects.pdf", grob = g_hist_census, height = 2)
    save_th("varied_thrush_census_hist_points.pdf", grob = g_hist_census_points, height = 2)
    
    save_th("varied_thrush_elevation_hist.pdf", g_hist_elev, height = 2)
    save_th("varied_thrush_slopoe_hist.pdf", g_hist_slope, height = 2)
    save_th("varied_thrush_aspect_hist.pdf", g_hist_aspect, height = 2)
    save_th("varied_thrush_elevation_hist_cont.pdf", g_hist_elev_cond, height = 2)
    save_th("varied_thrush_terrain_hist.pdf", g_hist_terrain, height = 2)
  }
}




## ---- save_terrain_maps ----
{
  if (save_images)
  {
    save_th_map("varied_thrush_map_elevation.png", map_elev)
    save_th_map("varied_thrush_map_slope.png", map_slope)
    save_th_map("varied_thrush_map_aspect.png", map_aspect)
    save_th_map("varied_thrush_map_terrain.png", map_terrain, width = 3)
  }
}