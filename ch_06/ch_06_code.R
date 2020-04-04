# ---- packages ----
# 
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

source(here("/data/environment_vars.R"))

# CRS for the raster and point data
crs_dat = "+proj=aea +lat_1=46 +lat_2=48 +lat_0=44 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Save output images?
save_images = FALSE




# ---- elevation data ----

dem = raster(file.path(book_data, "elev"))
proj4string(dem) = crs_dat

dem_m = dem
dem_m[] = dem[] * 1e3

dat_brick = brick(dem_m, terrain(dem_m, opt = c("slope", "aspect"), unit = "radians"))

names(dat_brick)[1] = "elevation"
terrain_brick = st_as_stars(dat_brick)

r_t = raster(extent(dat_brick), resolution = 5000)
terrain_brick_rs = st_as_stars(resample(dat_brick, raster(extent(dat_brick), resolution = 5000)))

# resamplign to make plotting faster
# terrain_brick_rs = st_as_stars(resample(dat_brick, raster(extent(dat_brick), resolution = 5000)))





#  ---- varied thrush data ----
thrush_dt =
  fread(file.path(book_data, "vath_2004.csv"), header = TRUE)

# thrush_dt[, present := VATH > 0]

thrush_dt[, present := ifelse(VATH == 0, "absent", "present")]


thrush_sf = st_as_sf(
  thrush_dt,
  coords = c("EASTING", "NORTHING"),
  crs = crs_dat)

thrush_sf = cbind(thrush_sf, extract(dat_brick, thrush_dt[, .(x = EASTING, y = NORTHING)]))



# ---- correlogram function ----
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

    #note alternative is for P-value, so only 'significant if positive autocorrelation
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




# ---- plotting function ----
#
plot_ff_correlogram = function(cgram, title, subttl = "", thm = theme_light(), ci_lty = 2)
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


# ---- plotting params ----
# Thrush data histogram params
# 
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
# 
# 
# # Map color and legend params
# map_sf_vir = scale_fill_viridis(
#   na.value = "transparent",
#   guide =
#     guide_colorbar(
#       title.position = "top",
#       barwidth = unit(0.6, "npc")))
# 
# # Map scale params  
# map_x = scale_x_discrete(expand=c(0,0))
# map_y = scale_y_discrete(expand=c(0,0))
# 
# gg_rast_gs = ggplot() +
#   coord_equal() +
#   scale_fill_viridis_c(
#     na.value = "transparent",
#     guide =
#       guide_colorbar(
#         title.position = "top",
#         barwidth = unit(0.6, "npc"))) +
#   scale_x_discrete(expand=c(0,0)) +
#   scale_y_discrete(expand=c(0,0)) +
#   theme_map() +
#   theme(
#     legend.position = "bottom",
#     legend.direction = "horizontal",
#     axis.title = element_blank())

# dimensions for output files
map_panel_width = 6.5
map_panel_height = 8
map_res = 250

fig_panel_width = 9
fig_panel_height_1 = 3



# ---- covariate pair plot ----
pairs.panels(data.frame(thrush_sf)[, c("elevation", "slope", "aspect")])



# ---- census histograms ----

g_hist_census = 
  ggplot(
    aggregate(VATH ~ TRANSECT, data = thrush_sf, sum), aes(x = VATH)) + 
  gm_hist_1 +
  labs(
    title = "Varied Thrush Census", 
    subtitle = "Counts aggregated by transect") +
  xlab("Census Count")



# ---- terrain histograms ----

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

# Conditional histograms
gm_hist_thrush_present = ggplot(subset(thrush_sf, VATH == 1)) + gm_thrush_hist_3
gm_hist_thrush_absent = ggplot(subset(thrush_sf, VATH == 0)) + gm_thrush_hist_3

g_hist_elev_present = 
  gm_hist_thrush_present + aes(x = elevation) +
  ggtitle("Sample Point Elevation: Present") + 
  xlab("Elevation (m)")

g_hist_elev_absent = 
  gm_hist_thrush_absent + aes(x = elevation) +
  ggtitle("Sample Point Elevation: Absent") + 
  xlab("Elevation (m)")




# ---- thrush map ----

gm_hist_thrush_present = ggplot(subset(thrush_sf, VATH == 1)) + gm_thrush_hist_3
gm_hist_thrush_absent = ggplot(subset(thrush_sf, VATH == 0)) + gm_thrush_hist_3

g_hist_elev_present = 
  gm_hist_thrush_present + aes(x = elevation) +
  ggtitle("Sample Point Elevation: Present") + 
  xlab("Elevation (m)")

g_hist_elev_absent = 
  gm_hist_thrush_absent + aes(x = elevation) +
  ggtitle("Sample Point Elevation: Absent") + 
  xlab("Elevation (m)")



# ---- center data ----
thrush_sf$elevation_sc <- scale(thrush_sf$elevation, center = T, scale = T)
thrush_sf$slope_sc <- scale(thrush_sf$slope, center = T, scale = T)
thrush_sf$aspect_sc <- scale(thrush_sf$aspect, center = T, scale = T)



# ---- model 1 center ----
fit_elev = glm(
  VATH ~ elevation_sc, 
  family = "binomial", 
  data = thrush_sf)
summary(fit_elev)



# ---- model 2 center ----
fit_terrain = glm(
  VATH ~ elevation_sc + slope_sc + aspect_sc, 
  family = "binomial", 
  data = thrush_sf)
summary(fit_terrain)



# ---- model 3 center ----
fit_elev_poly =  glm(
  VATH ~ elevation_sc + I(elevation_sc^2), 
  family = "binomial", 
  data = thrush_sf)
summary(fit_elev_poly)





# ---- save thrush maps ----

if(FALSE)
{
  bb_inset = st_bbox(
    c(xmin = 170000,
      xmax = 210000,
      ymin = 500000,
      ymax = 551000), 
    crs = crs_dat)
  
  dat_br = terrain_brick
  # dat_br = terrain_brick_rs
  st_crs(terrain_brick_rs) = crs_dat
  cols = gg_terrain_grayscale
  
  thrush_inset = st_crop(thrush_sf, bb_inset)
  elev_inset = st_crop(terrain_brick[, , , 1], bb_inset)
  
  gg_thrush_start = 0.1

  gg_terrain_thrush = 
    scale_fill_gradientn(
      colours = gray.colors(3, start = gg_thrush_start),
      na.value = "transparent",
      guide = map_cb)
  
  cols = gg_terrain_thrush
  
  thrush_map = gg_map + geom_stars(data = dat_br[, , , 1], show.legend = FALSE) + labs(fill = "Elevation") + cols +
    geom_sf(aes(colour = present), data = thrush_sf, size = 3, pch = 16, show.legend = FALSE) +
    geom_sf(aes(colour = present), data = thrush_sf, size = 3, pch = 16, show.legend = FALSE) 
  
  thrush_inset = gg_map + geom_stars(data = elev_inset, show.legend = FALSE) + labs(fill = "Elevation") + cols +
    geom_sf(aes(colour = present), data = thrush_inset, size = 2, pch = 16, show.legend = TRUE) +
    geom_sf(aes(colour = present), data = thrush_inset, size = 2, pch = 16, show.legend = FALSE) +
    theme(legend.title = element_blank())
  thrush_inset
  inset = geom_sf(data = st_as_sfc(bb_inset), fill = "transparent", col = rgb(0, 0.3, 0.001), size = 1.9)
  
  # png(file.path(fig_dir, "varied_thrush_inset.png"), units = "in", height = map_height, width = map_width, res =  map_res)
  # print(thrush_inset)
  # dev.off()
  # # 
  # png(file.path(fig_dir, "varied_thrush_map.png"), units = "in", height = map_height, width = map_width, res =  map_res)
  # print(thrush_map)
  # dev.off()
  
  png(file.path(fig_dir, "varied_thrush_map_and_inset.png"), units = "in", height = map_height, width = 2 * map_width, res =  map_res)
  print(plot_grid(thrush_map + inset, thrush_inset, nrow = 1))
  dev.off()
}



# ---- save histograms ----
if (FALSE)
{
    
    pdf(file.path(fig_dir, "varied_thrush_pair_plot.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
    print(pairs.panels(data.frame(thrush_sf)[, c("elevation", "slope", "aspect")]))
    dev.off()
    
    pdf(file.path(fig_dir, "varied_thrush_census_hist.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
    print(g_hist_census)
    dev.off()
    
    pdf(file.path(fig_dir, "varied_thrush_elevation_hist.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
    print(g_hist_elev)
    dev.off()
    
    pdf(file.path(fig_dir, "varied_thrush_elevation_hist_cond.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
    print(plot_grid(g_hist_elev_present, g_hist_elev_absent, nrow = 1))
    dev.off()
    
    pdf(file.path(fig_dir, "varied_thrush_slope_hist.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
    print(g_hist_slope)
    dev.off()
    
    pdf(file.path(fig_dir, "varied_thrush_aspect_hist.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
    print(g_hist_aspect)
    dev.off()
    
    pdf(file.path(fig_dir, "varied_thrush_terrain_all_hist.pdf"), width = fig_panel_width, height = 3.5 * fig_panel_height_1)
    print(plot_grid(g_hist_elev, g_hist_slope, g_hist_aspect, ncol = 1))
    dev.off()
}


# ---- save terrain maps ----
if(FALSE)
{
  dat_br = terrain_brick
  dat_br = terrain_brick_rs
  cols = gg_terrain_grayscale
  cols = gg_terrain_terr
  
  g_elev = gg_map + geom_stars(data = dat_br[, , , 1]) + labs(fill = "Elevation") + cols
  g_slope = gg_map + geom_stars(data = dat_br[, , , 2]) + labs(fill = "Slope") + cols
  g_aspect = gg_map + geom_stars(data = dat_br[, , , 3]) + labs(fill = "Aspect") + cols
  
  png(file.path(fig_dir, "varied_thrush_map_elevation.png"), 
      width = map_panel_width, height = map_panel_height, units = "in", res = map_res)
  print(g_elev)
  dev.off()
  
  png(file.path(fig_dir, "varied_thrush_map_aspect.png"), 
      width = map_panel_width, height = map_panel_height, units = "in", res = map_res)
  print(g_aspect)
  dev.off()
  
  png(file.path(fig_dir, "varied_thrush_map_slope.png"),
      width = map_panel_width, height = map_panel_height, units = "in", res = map_res)
  print(g_slope)
  dev.off()
  
  png(file.path(fig_dir, "varied_thrush_map_terrain.png"), 
      width = 3 * map_panel_width, height = map_panel_height, units = "in", res = map_res)
  print(plot_grid(g_elev, g_slope, g_aspect, nrow = 1))
  dev.off()
}

