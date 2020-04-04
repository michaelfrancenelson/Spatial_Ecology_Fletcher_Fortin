{
  require(here)
  require(stars)
  require(sf)
  require(ggplot2)
  require(viridis)
  require(spData)
  require(raster)
  require(cowplot)
  require(sp)
  
  source(here("/data/environment_vars.R"))
  
  dem_crs_m = "+proj=aea +lat_1=46 +lat_2=48 +lat_0=44 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
}

# graphics params ----
{
  hist_col = "black"
  hist_fill = "steelblue"
  hist_fill_2 = rgb(0, 0.6, 0.9)
  hist_alpha = 0.2
  
  gm_hist_1 = 
    geom_histogram(binwidth = 1, color = hist_col, fill = hist_fill, alpha = hist_alpha)
  gm_thrush_hist_1 =
    geom_histogram(bins = 30, color = hist_col, fill = hist_fill, alpha = hist_alpha)
  gm_thrush_hist_2 =
    geom_histogram(bins = 30, color = hist_col, fill = hist_fill_2, alpha = hist_alpha)
  gm_thrush_hist_3 =
    geom_histogram(bins = 18, color = hist_col, fill = hist_fill_2, alpha = hist_alpha)
  
  # Map color and legend params
  legend_width = 0.6
  
  map_cb =
    guide_colorbar(
      title.position = "top",
      barwidth = unit(legend_width, "npc"))
  
  gg_terrain_terr = 
    scale_fill_gradientn(
      colours = terrain.colors(20),
      na.value = "transparent",
      guide = map_cb)
  
  gg_terrain = 
    scale_fill_gradientn(
      colours = terrain.colors(20),
      na.value = "transparent",
      guide = map_cb)
  
  gg_terrain_vir = 
    scale_fill_viridis(
    na.value = "transparent",
    guide = map_cb)
  
  gg_terrain_grayscale = 
    scale_fill_gradientn(
      colours = gray.colors(3),
      na.value = "transparent",
      guide = map_cb)
  
  gg_terrain_terr = 
    scale_fill_gradientn(
      colours = terrain.colors(20),
      na.value = "transparent",
      guide = map_cb)
  
  gg_terrain_heat = 
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
  
  map_width = 6.5
  map_height = 8
  map_res = 250
}



# Varied Thrush data ----
{
  thrush_pts <- read.csv(file.path(book_data, "vath_2004.csv"), header = TRUE)
  
  thrush_pts = 
    within(thrush_pts,
           {
             census = factor(VATH)
             levels(census) = c("absent", "present")
           }
    )
  head(thrush_pts)
  coordinates(thrush_pts) = ~ EASTING + NORTHING
  proj4string(thrush_pts) = dem_crs_m
  
  thrush_sf = st_as_sf(thrush_pts)
}

# Raster elevation data ----
{
  dem_crs_m = "+proj=aea +lat_1=46 +lat_2=48 +lat_0=44 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  dem = raster(file.path(book_data, "elev"))
  proj4string(dem) = dem_crs_m
  
  dem_m = dem
  dem_m[] = dem[] * 1e3
  
  dat_brick = brick(dem_m, terrain(dem_m, opt = c("slope", "aspect"), unit = "radians"))
  names(dat_brick)[1] = "elevation"
  dat_stars = st_as_stars(dat_brick)
  
  rm(dem, dem_m) 
}


# Terrain maps ----
if (FALSE)
{
  
  r_t = raster(extent(dat_brick), resolution = 5000)
  dat_rs = resample(dat_brick, raster(extent(dat_brick), resolution = 5000))
  
  dat_rs_stars = st_as_stars(dat_rs)
  
  gg_map + geom_stars(data = dat_rs_stars[, , , 2]) + labs(fill = "Slope") +
    gg_terrain_grayscale
  gg_map + geom_stars(data = dat_rs_stars[, , , 2]) + labs(fill = "Slope") + 
    gg_terrain_grayscale
  gg_map + geom_stars(data = dat_rs_stars[, , , 2]) + labs(fill = "Slope") + 
    gg_terrain
  gg_map + geom_stars(data = dat_rs_stars[, , , 2]) + labs(fill = "Slope") + 
    gg_terrain_heat
  
  
  
  
  
}



# Thrush sampling maps -----
if(FALSE)
{
  bb_inset = st_bbox(
    c(xmin = 197000,
      xmax = 206000,
      ymin = 497000,
      ymax = 509000), 
    crs = dem_crs_m)
  
  thrush_inset = st_crop(thrush_sf, bb_inset)
  elev_inset = st_crop(dat_stars[, , , 1], bb_inset)
  
  gg_thrush_start = 0.1
  
  g_inset = ggplot() + 
    geom_stars(aes(x, y, fill = elevation), data = elev_inset) +
    scale_fill_gradientn(colours = gray.colors(2, start = gg_thrush_start), na.value = "transparent") +
    geom_sf(aes(colour = census), data = thrush_inset, size = 3, pch = 16) +
    geom_sf(aes(colour = census), data = thrush_inset, size = 3, pch = 21) +
    theme_map()
  
  g_thrush = ggplot() + 
    geom_stars(aes(x, y, fill = elevation), data = dat_stars[, , , 1]) +
    scale_fill_gradientn(colours = gray.colors(2, start = gg_thrush_start), na.value = "transparent") +
    geom_sf(aes(colour = census), data = thrush_sf, size = 3, pch = 16) +
    geom_sf(aes(colour = census), data = thrush_sf, size = 3, pch = 21) +
    theme_map() +
    geom_sf(data = st_as_sfc(bb_inset), fill = "transparent")
  
  png(file.path(fig_dir, "varied_thrush_sample_inset.png"), units = "in", height = map_height, width = map_width, res =  map_res)
  print(g_inset)
  dev.off()
  
  png(file.path(fig_dir, "varied_thrush_sample_all.png"), units = "in", height = map_height, width = map_width, res =  map_res)
  print(g_thrush)
  dev.off()
  
  png(file.path(fig_dir, "varied_thrush_sample_map.png"), units = "in", height = map_height, width = 2 * map_width, res =  map_res)
  print(plot_grid(g_thrush, g_inset, nrow = 1))
  dev.off()
  
  st_proj_info(dat_stars)
  st_crs(dat_stars)
  st_crs(bb_inset)
}


