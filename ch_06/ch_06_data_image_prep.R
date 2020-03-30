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
  gg_rast = ggplot() + 
    coord_equal() + 
    scale_fill_viridis(
      na.value = "transparent",
      guide = 
        guide_colorbar(
          title.position = "top",
          barwidth = unit(0.6, "npc"))) + 
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    theme_map() +
    theme(
      legend.position = "bottom", 
      legend.direction = "horizontal",
      axis.title = element_blank())
  
  gg_rast_gs = ggplot() + 
    coord_equal() + 
    scale_fill_gradient(
      gray.colors(2),
      na.value = "transparent",
      guide = 
        guide_colorbar(
          title.position = "top",
          barwidth = unit(0.6, "npc"))) + 
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
# if (FALSE)
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

  
  r_t = raster(extent(dat_brick), resolution = 1000)
  dat_rs = resample(dat_brick, raster(extent(dat_brick), resolution = 1000))
  
  dat_rs_stars = st_as_stars(dat_rs)
    
  df_elev = as.data.frame(subset(dat_rs, 1), xy = TRUE)
  df_slpe = as.data.frame(subset(dat_rs, 2), xy = TRUE)
  df_aspc = as.data.frame(subset(dat_rs, 3), xy = TRUE)
  
  g_elev = gg_rast + geom_stars(data = dat_stars[, , , 1], downsample = c(4, 4, 4)) + labs(fill = "Elevation")
  g_slop = gg_rast + geom_stars(data = dat_stars[, , , 2]) + labs(fill = "Slope")
  g_aspt = gg_rast + geom_stars(data = dat_stars[, , , 3]) + labs(fill = "Aspect")
  
  g_aspt = gg_rast + geom_stars(data = dat_stars[, , , 3]) + labs(fill = "Aspect")
  
  head(df_elev)
  class(df_elev$elevation)
  
  
  head(df_elev)
  ggplot() + geom_raster(aes(x, y, fill = elevation), data = df_elev) +
    scale_fill_gradientn(colours = gray.colors(3), na.value = "transparent")

  
  
    
  ggplot() + 
    geom_stars(
      aes(x, y, fill = elevation), 
      data = dat_rs_stars[, , , 1]) +
    scale_fill_gradientn(colours = gray.colors(3), na.value = "transparent")
  
  
  
  
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
  
  g_inset = ggplot() + 
    geom_stars(aes(x, y, fill = elevation), data = elev_inset) +
    scale_fill_gradientn(colours = gray.colors(2, start = 0.7), na.value = "transparent") +
    geom_sf(aes(colour = census), data = thrush_inset, size = 3, pch = 16) +
    geom_sf(aes(colour = census), data = thrush_inset, size = 3, pch = 21) +
    theme_map()
  g_thrush = ggplot() + 
    geom_stars(aes(x, y, fill = elevation), data = dat_stars[, , , 1]) +
    scale_fill_gradientn(colours = gray.colors(2, start = 0.7), na.value = "transparent") +
    geom_sf(aes(colour = census), data = thrush_sf, size = 3, pch = 16) +
    geom_sf(aes(colour = census), data = thrush_sf, size = 3, pch = 21) +
    theme_map() +
    geom_sf(data = st_as_sfc(bb_inset), fill = "transparent")
  
  png(file.path(fig_dir, "thrush_sample_inset.png"), units = "in", height = map_height, width = map_width, res =  map_res)
  print(g_inset)
  dev.off()
  
  
  png(file.path(fig_dir, "thrush_sample_all.png"), units = "in", height = map_height, width = map_width, res =  map_res)
  print(g_thrush)
  dev.off()
  
  
  png(file.path(fig_dir, "thrush_sample_map.png"), units = "in", height = map_height, width = 2 * map_width, res =  map_res)
  print(plot_grid(g_thrush, g_inset, nrow = 1))
  dev.off()
  
  
  st_proj_info(dat_stars)
  st_crs(dat_stars)
  st_crs(bb_inset)
    
}


