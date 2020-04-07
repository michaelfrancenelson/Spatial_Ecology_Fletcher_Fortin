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


## ---- center_data ----
{
  thrush_sf$elevation_sc = scale(thrush_sf$elevation, center = T, scale = T)
  thrush_sf$slope_sc = scale(thrush_sf$slope, center = T, scale = T)
  thrush_sf$aspect_sc = scale(thrush_sf$aspect, center = T, scale = T)
}

# 
# ##
#   plotting_params 
# {
#   # Thrush data histogram params
#   hist_col = "black"
#   hist_fill = "steelblue"
#   hist_fill_2 = rgb(0, 0.6, 0.9)
#   hist_alpha = 0.2
#   
#   elev_tmp = c(terrain_brick[, , , 1]$elevation)
#   rng = range(elev_tmp[elev_tmp > 0], na.rm = T)
#   
#   gm_hist_1 = 
#     geom_histogram(binwidth = 1, color = hist_col, fill = hist_fill, alpha = hist_alpha)
#   gm_thrush_hist_1 =
#     geom_histogram(bins = 30, color = hist_col, fill = hist_fill, alpha = hist_alpha)
#   gm_thrush_hist_2 =
#     geom_histogram(bins = 30, color = hist_col, fill = hist_fill_2, alpha = hist_alpha)
#   gm_thrush_hist_3 =
#     geom_histogram(bins = 18, color = hist_col, fill = hist_fill_2, alpha = hist_alpha)
#   
#   # Map color and legend params
#   legend_width = 0.4
#   
#   map_cb =
#     guide_colorbar(
#       title.position = "left",
#       barwidth = unit(legend_width, "npc"))
#   
#   gg_thrush_start = 0.1
#   
#   gg_thrush_elevation = 
#     scale_fill_gradientn(
#       colours = gray.colors(3, start = gg_thrush_start),
#       na.value = "transparent",
#       guide = map_cb,
#       limits = c(rng[1], rng[2])
#     )
#   
#   gg_map_vir = 
#     scale_fill_viridis(
#       na.value = "transparent",
#       guide = map_cb)
#   
#   gg_map_grayscale = 
#     scale_fill_gradientn(
#       colours = gray.colors(3),
#       na.value = "transparent",
#       guide = map_cb)
#   
#   gg_map_terr = 
#     scale_fill_gradientn(
#       colours = terrain.colors(20),
#       na.value = "transparent",
#       guide = map_cb)
#   
#   gg_map_heat = 
#     scale_fill_gradientn(
#       colours = heat.colors(3),
#       na.value = "transparent",
#       guide = map_cb)
#   
#   gg_map = ggplot() +
#     coord_equal() +
#     scale_x_discrete(expand=c(0,0)) +
#     scale_y_discrete(expand=c(0,0)) +
#     theme_map() +
#     theme(
#       legend.position = "bottom", 
#       legend.direction = "horizontal",
#       axis.title = element_blank())
#   
#   rm(rng, elev_tmp)
# }
# 
# 
# 
# 
# ## ---- covariate_pair_plot ----
# {
#   pairs.panels(
#     data.frame(thrush_sf)[, 
#                           c("elevation", "slope", "aspect")])
# }
# 
# 
# 
# 
# ## ---- census_histograms ----
# {
#   require(grid)
#   require(ggplotify)
#   require(ggplot2)
#   
#   pplot = as.ggplot(
#     ~pairs.panels(
#       data.frame(thrush_sf)[, c("elevation", "slope", "aspect")]))
#   
#   g_hist_census = 
#     ggplot(
#       aggregate(
#         VATH ~ TRANSECT, 
#         data = thrush_sf, sum),
#       aes(x = VATH)) + 
#     gm_hist_1 +
#     labs(
#       title = "Varied Thrush Census", 
#       subtitle = "Counts aggregated by transect") +
#     xlab("Census Count")
#   
#   g_hist_census_points = 
#     ggplot(
#       aggregate(
#         VATH ~ TRANSECT * POINT, 
#         data = thrush_sf, sum), 
#       aes(x = VATH)) + 
#     gm_hist_1 +
#     labs(
#       title = "Varied Thrush Census", 
#       subtitle = "Counts aggregated by sample points in transects") +
#     xlab("Census Count")
# }
# 
# ## ---- terrain_histograms ----
# {
#   gm_hist_thrush = ggplot(thrush_sf) + gm_thrush_hist_2
#   
#   g_hist_elev = 
#     gm_hist_thrush + aes(x = elevation) +
#     ggtitle("Sample Point Terrain: Elevation") + 
#     xlab("Elevation (m)")
#   
#   g_hist_slope = 
#     gm_hist_thrush +  aes(x = slope) +
#     ggtitle("Sample Point Terrain: Slope") + 
#     xlab("Slope (m / km)")
#   
#   g_hist_aspect = 
#     gm_hist_thrush + aes(x = aspect) + 
#     ggtitle("Sample Point Terrain: Aspect") + 
#     xlab("Aspect (radians)")
#   
#   g_hist_terrain = plot_grid(
#     g_hist_elev,
#     g_hist_slope, 
#     g_hist_aspect, ncol = 1)
#   
#   # Conditional histograms
#   gm_hist_thrush_present = 
#     ggplot(subset(thrush_sf, VATH == 1)) +
#     gm_thrush_hist_3
#   gm_hist_thrush_absent = 
#     ggplot(subset(thrush_sf, VATH == 0)) +
#     gm_thrush_hist_3
#   
#   g_hist_elev_present = 
#     gm_hist_thrush_present + aes(x = elevation) +
#     labs(
#       title ="Sample Point Elevation",
#       subtitle = "Thrushes Present") + 
#     xlab("Elevation (m)")
#   
#   g_hist_elev_absent = 
#     gm_hist_thrush_absent + aes(x = elevation) +
#     labs(
#       title = "Sample Point Elevation", 
#       subtitle = "Thrushes Absent") + 
#     xlab("Elevation (m)")
#   
#   g_hist_elev_cond = plot_grid(
#     g_hist_elev_present, g_hist_elev_absent, nrow = 1)
#   
#   
# }
# 
# 
# 
# 


