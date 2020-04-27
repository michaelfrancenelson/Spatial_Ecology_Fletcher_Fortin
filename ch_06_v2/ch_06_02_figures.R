## ---- plotting_params ----
{
  # Thrush data histogram params
  hist_col = "black"
  hist_fill = "steelblue"
  hist_fill_2 = rgb(0, 0.6, 0.9)
  hist_alpha = 0.2
  
  
  elev_tmp = terrain_brick$elevation
  rng = range(elev_tmp[elev_tmp[] > 0], na.rm = T)
  
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
pairs.panels(data.frame(thrush_sf)[, c("elevation", "slope", "aspect")])




## ---- census_histograms ----
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




## ---- terrain_histograms ----
gm_hist_thrush = ggplot(thrush_sf) + gm_thrush_hist_2


summary(thrush_sf$VATH)

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



# ---- model_plots ----
{
  newdata = data.frame(
    elevation_cen =   
      seq(
        min(thrush_sf$elevation_cen), 
        max(thrush_sf$elevation_cen), 
        length = 25))
  
  # Use model to predict values for different elevations:
  # type = response for predicted probabilities
  glm.pred = predict(
    fit_elev_cen, 
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




## ---- terrain_stars ----
terrain_stars = st_as_stars(terrain_brick)
terrain_stars_rs = st_as_stars(resample(terrain_brick, raster(extent(terrain_brick), resolution = 5000)))
st_crs(terrain_stars) = crs_dat
st_crs(terrain_stars_rs) = crs_dat


## ---- terrain_maps ----
{
  # original raster set for hi-resolution maps
  dat_br = terrain_stars
  
  # reduced-resolution raster set to speed up plotting
  dat_br = terrain_stars_rs
  
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
  dat_br = terrain_stars
  
  # To speed  up test plots
  dat_br = terrain_stars_rs
  
  thrush_inset = st_crop(thrush_sf, bb_inset)
  elev_inset = st_crop(terrain_stars[, , , 1], bb_inset)
  
  thrush_inset = st_crop(thrush_sf, bb_inset)
  elev_inset = st_crop(terrain_stars[, , , 1], bb_inset)
  
  cols = gg_thrush_elevation
  sc_census = scale_color_manual(name = "Varied Thrush", values = c(2, 3), breaks = 2:3)
  # sc_census = scale_color_manual(name = "Varied Thrush", values = c(2, 3))
  
  thrush_inset_poly = 
    geom_sf(data = st_as_sfc(bb_inset),
            fill = "transparent",
            col = rgb(0, 0.3, 0.001), size = 1.9)
  
  thrush_map = 
    gg_map + 
    geom_stars(data = dat_br[, , , 1]) +
    geom_sf(
      aes(colour = (VATH > 0)),
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
      aes(colour = (VATH > 0)),
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