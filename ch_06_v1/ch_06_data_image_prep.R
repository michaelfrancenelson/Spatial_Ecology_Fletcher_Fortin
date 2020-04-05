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
  # dem_crs_m = "+proj=aea +lat_1=46 +lat_2=48 +lat_0=44 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  
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
  dat_rs
  dem
  dat_rs_stars = st_as_stars(dat_rs)
    
  # df_elev = as.data.frame(subset(dat_rs, 1), xy = TRUE)
  # df_slpe = as.data.frame(subset(dat_rs, 2), xy = TRUE)
  # df_aspc = as.data.frame(subset(dat_rs, 3), xy = TRUE)
  
  g_elev = gg_rast + geom_stars(data = dat_stars[, , , 1], downsample = c(4, 4, 4)) + labs(fill = "Elevation")
  g_slop = gg_rast + geom_stars(data = dat_stars[, , , 2]) + labs(fill = "Slope")
  g_aspt = gg_rast + geom_stars(data = dat_stars[, , , 3]) + labs(fill = "Aspect")
  g_elev
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

{
  
  dem = raster(file.path(book_data, "elev"))
  proj4string(dem) = crs_dat
  
  dem[] = dem[] * 1000
  
  terrain_brick = brick(dem, terrain(dem, opt = c("slope", "aspect"), unit = "radians"))
  
  # brick appends '_km' to the elevetaion layer name:
  names(terrain_brick)
  names(terrain_brick)[1] = "elev"
  
  terrain_stars = st_as_stars(terrain_brick)
  thrush_dt <- 
    fread(file.path(book_data, "vath_2004.csv"), header = TRUE)
  
  thrush_sf = st_as_sf(
    thrush_dt,
    coords = c("NORTHING", "EASTING"), 
    crs = crs_dat)
  
  coords = thrush_dt[, .(x = EASTING, y = NORTHING)]
  thrush_sf = cbind(thrush_sf, extract(terrain_brick, coords))
  
  
  ff_correlogram <- function(
    point_coords, 
    point_data,
    max_dist = NULL,
    bin_size = NULL,
    alpha = 0.05)
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
      
      neigh <- 
        spdep::dnearneigh(
          x = point_coords,
          d1 = d_lower,
          d2 = d_upper, 
          longlat = F)
      
      wts <- 
        nb2listw(
          neighbours = neigh, 
          style = 'B', 
          zero.policy = T)
      
      #note alternative is for P-value, so only 'significant if positive autocorrelation
      mor.i <- 
        moran.mc(
          x = point_data, 
          listw = wts, 
          nsim = 200, 
          alternative = "greater", 
          zero.policy = T) 
      
      # null envelope	
      null_env = quantile(
        mor.i$res, 
        probs = c(alpha / 2, 1 - alpha / 2), 
        na.rm = T)
      
      moran_out[i, ] = 
        c(
          dist_class,
          mor.i$statistic,
          mor.i$p.value,
          null_env
        )
    }
    return(moran_out)
  }
  
  
  point_coords = st_coordinates(thrush_sf)
  point_data    = thrush_sf$elev
  bin_size = 300
  max_dist = 10000
  corr_1 = ff_correlogram(point_coords, point_data)
  head(corr_1)
  with(corr_1, matplot(x = dist_class, y = cbind(model_I, null_I_lower, null_I_upper), type = "l"))
  
  pairs.panels(data.frame(thrush_sf)[, c("elev", "slope", "aspect")])

  
  g_hist_census = 
    ggplot(
      aggregate(VATH ~ TRANSECT, data = thrush_sf, sum), aes(x = VATH)) + 
    gm_hist_1 +
    labs(
      title = "Varied Thrush Census", 
      subtitle = "Counts aggregated by transect") +
    xlab("Census Count")
  
  print(g_hist_census)

  
  gm_hist_thrush = ggplot(thrush_sf) + gm_thrush_hist_2
  g_hist_elev = 
    gm_hist_thrush + aes(x = elev) +
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
  
  plot_grid(g_hist_elev, g_hist_slope, g_hist_aspect, ncol = 1)

  
  gm_hist_thrush_present = ggplot(subset(thrush_sf, VATH == 1)) + gm_thrush_hist_3
  gm_hist_thrush_absent = ggplot(subset(thrush_sf, VATH == 0)) + gm_thrush_hist_3
  
  g_hist_elev_present = 
    gm_hist_thrush_present + aes(x = elev) +
    ggtitle("Sample Point Elevation: Present") + 
    xlab("Elevation (m)")
  
  g_hist_elev_absent = 
    gm_hist_thrush_absent + aes(x = elev) +
    ggtitle("Sample Point Elevation: Absent") + 
    xlab("Elevation (m)")
  
  plot_grid(g_hist_elev_present, g_hist_elev_absent, nrow = 1)

  
  pdf(here(slide_img_dir, "varied_thrush_pair_plot.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
  print(pairs.panels(data.frame(thrush_sf)[, c("elev", "slope", "aspect")]))
  dev.off()
  
  pdf(here(slide_img_dir, "varied_thrush_census_hist.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
  print(g_hist_census)
  dev.off()
  
  pdf(here(slide_img_dir, "varied_thrush_elevation_hist.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
  print(g_hist_elev)
  dev.off()
  
  pdf(here(slide_img_dir, "varied_thrush_elevation_hist_cond.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
  print(plot_grid(g_hist_elev_present, g_hist_elev_absent, nrow = 1))
  dev.off()
  
  pdf(here(slide_img_dir, "varied_thrush_slope_hist.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
  print(g_hist_slope)
  dev.off()
  
  pdf(here(slide_img_dir, "varied_thrush_aspect_hist.pdf"), width = fig_panel_width, height = 2 * fig_panel_height_1)
  print(g_hist_aspect)
  dev.off()
  
  pdf(here(slide_img_dir, "varied_thrush_terrain_all_hist.pdf"), width = fig_panel_width, height = 3.5 * fig_panel_height_1)
  print(plot_grid(g_hist_elev, g_hist_slope, g_hist_aspect, ncol = 1))
  dev.off()
  
  
  thrush_dt[,hist(
    by(VATH, TRANSECT, sum),
    main = "Varied Thrush Count Per Transect",
    xlab = "Census Count")]
  
  
  ggplot(aggregate(VATH ~ TRANSECT, data = thrush_dt, sum), aes(x = VATH)) + geom_histogram(binwidth = 1, fill = gray(0, 0.5), col = "green")
  
  ggplot(thrush_dt) + geom_histogram(data = by(VATH, TRANSECT, sum))
  
  pdf(file.path(fig_dir, "varied_thrush_transect_count_hist.pdf"), width = 7, height = 4)
  with(thrush_sf, {
    hist(
      by(VATH, TRANSECT, sum),
      main = "Varied Thrush Count Per Transect",
      xlab = "Census Count")
  })
  dev.off()
  
  pdf(file.path(fig_dir, "varied_thrush_transect_data.pdf"), width = 12, height = 2)
  grid.table(head(point.data))
  dev.off()
  
  summary(point.data[, c(4, 7:9)])
  {
    pdf(file.path(fig_dir, "varied_thrush_transect_terrain_hist.pdf"), width = 8, height = 7)
    par(mfrow = c(3, 1))
    hist(point.data[, c(7)], main = "Elevation at sample sites", xlab = "elevation (km)")
    hist(point.data[, c(8)], main = "Slope at sample sites", xlab = "elevation (km)")
    hist(point.data[, c(9)], main = "Aspect at sample sites", xlab = "elevation (km)")
    dev.off()
  }
  
  
  
  
  thrush_dt <- 
    fread(file.path(book_data, "vath_2004.csv"), header = TRUE)
  
  thrush_sf = st_as_sf(
    thrush_dt,
    coords = c("NORTHING", "EASTING"), 
    crs = crs_dat)
}



```{r}
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
```






<!-- ## Elevation/terrain data -->
  <!-- ```{r data import} -->
  
  <!-- dem_crs_m = "+proj=aea +lat_1=46 +lat_2=48 +lat_0=44 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" -->
  
  <!-- dem = raster(file.path(book_data, "elev")) -->
  <!-- proj4string(dem) = dem_crs_m -->
  <!-- ``` -->
  
  <!-- The book's supplied raster file has elevation in km, but it's easier to work with meters. -->
  <!-- ```{r} -->
  
  <!-- dem_m = dem -->
  <!-- dem_m[] = dem[] * 1e3 -->
  
  <!-- dat_terrain = brick(dem_m, terrain(dem_m, opt = c("slope", "aspect"), unit = "radians")) -->
  <!-- names(dat_terrain)[1] = "elevation" -->
  <!-- dat_rast = st_as_stars(dat_terrain) -->
  
  <!-- dat_rast[, , 1] -->
  
  <!-- g_elev = gg_rast + geom_stars(data = dat_rast[, , , 1]) + labs(fill = "Elevation") -->
  <!-- g_slop = gg_rast + geom_stars(data = dat_rast[, , , 2]) + labs(fill = "Slope") -->
  <!-- g_aspt = gg_rast + geom_stars(data = dat_rast[, , , 3]) + labs(fill = "Aspect") -->
  
  <!-- ``` -->
  
  ## Terrain Maps
  ```{r}

if(FALSE)
{
  png(file.path(fig_dir, "thrush_elevation.png"), 
      width = map_panel_width, height = map_panel_height, units = "in", res = map_res)
  print(g_elev)
  dev.off()
  
  png(file.path(fig_dir, "thrush_aspect.png"), 
      width = map_panel_width, height = map_panel_height, units = "in", res = map_res)
  print(g_aspt)
  dev.off()
  
  png(file.path(fig_dir, "thrush_slope.png"),
      width = map_panel_width, height = map_panel_height, units = "in", res = map_res)
  print(g_slop)
  dev.off()
  
  png(file.path(fig_dir, "thrush_terrain.png"), 
      width = 3 * map_panel_width, height = map_panel_height, units = "in", res = map_res)
  print(plot_grid(g_elev, g_slop, g_aspt, nrow = 1))
  dev.off()
}

```


## Thrush data


```{r}
thrush_pts <- read.csv(file.path(book_data, "vath_2004.csv"), header = TRUE)

thrush_pts = 
  within(thrush_pts,
         {
           `census` = factor(VATH)
           levels(census) = c("absent", "present")
         }
  )
head(thrush_pts)
plot(x = EASTING, y = NORTHING, data = thrush_pts)

with(thrush_pts,
     
     plot(NORTHING, EASTING)
)

locator(2)

coordinates(thrush_pts) = ~ EASTING + NORTHING
proj4string(thrush_pts) = dem_crs_m
plot(thrush_pts)


thrush_sf = st_as_sf(thrush_pts)

```





# Overview

icorrelogram <- function(locations, z, binsize, max_dist)
{
  # package 'spdep' needed for dnearneigh() function.
  # Check whether it is installed:
  stopifnot(require(spdep))
  
  distbin <- seq(0, max_dist, by = binsize)
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
        na.rm = T) #95% null envelope
  }
  return(moran.results)
}

# plotting function
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

# elev = raster(file.path(book_data, "elev.gri"))
# elev = raster(file.path(book_data, "elev.grd"))


elev.terr <- terrain( elev, opt = c(" slope", "aspect"), unit = "radians")

proj4string(elev)=CRS(" +proj=aea +lat_1=46 +lat_2=48 +lat_0=44 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# Supplemental code version
# elev.terr <- terrain(elev, opt = c("slope", "aspect"))

# Book version
elev.terr <- terrain(elev, opt = c("slope", "aspect"), unit = "radians")
layers <- stack(elev, elev.terr)
layers <- brick(elev, elev.terr)
names(layers) <- c("elev", "slope", "aspect")
class(layers)


par(mfrow = c(1, 3))
plot(subset(layers, 1))
plot(subset(layers, 2))
plot(subset(layers, 3))



## Varied Thrush Data

#Point data with presence/absence
point.data <- read.csv(file.path(book_data, "vath_2004.csv"), header = TRUE)

dev.off()
plot(subset(layers, 1))

#plots the presence points
points(point.data[point.data$VATH == 0, c("EASTING", "NORTHING")], col = "black", pch = 21, cex = 0.8)

#plots the absence points
points(point.data[point.data$VATH == 1, c("EASTING", "NORTHING")], col = "red", pch = 16, cex = 0.7)



coords <- cbind(point.data$EASTING, point.data$NORTHING)
land.cov <- extract(x = layers, y = coords)
point.data <- cbind(point.data, land.cov)


# Aspatial logistic regression

## Data set-up

### Check for collinearity (correlations among predictors)

psych::pairs.padata.frame(thrush_sf)


cor(point.data[, c("elev", "slope", "aspect")], method = "pearson")
# pairs(point.data[, c("elev", "slope", "aspect")])
# the psych package implementatin looks much nicer
psych::pairs.panels(point.data[, c("elev", "slope", "aspect")])


### Center and scale predictors for modeling
point.data$elevs <- scale(point.data$elev, center = T, scale = T)
point.data$slopes <- scale(point.data$slope, center = T, scale = T)
point.data$aspects <- scale(point.data$aspect, center = T, scale = T)



## Model 1: elevation 
VATH.elev <- glm(
  VATH ~ elev, 
  family = "binomial", 
  data = point.data)
summary(VATH.elev)



## Model 2: additive - all factors
```{r}
VATH.all <- glm(
  VATH ~ elevs + slopes + aspects, 
  family = "binomial", 
  data = point.data)
summary(VATH.all)
```




## Model 3: polynomial elevation
```{r}
VATH.elev2 <- glm(
  VATH ~ elev + I(elev^2), 
  family = "binomial", 
  data = point.data)
summary(VATH.elev2)
```




## Model Selection

The polynomial elevation model has lowest AIC
```{r}
AIC(VATH.elev, VATH.all, VATH.elev2)
```


Extract coefficients and their SEs from the best model
```{r}
summary(VATH.elev2)$coef
glm.summary <- c(
  summary(VATH.elev2)$coef[2, 1], 
  summary(VATH.elev2)$coef[2, 2], 
  summary(VATH.elev2)$coef[3, 1], 
  summary(VATH.elev2)$coef[3, 2])
#inspect
glm.summary
```
<br><br>
  
  
  # Plot (aspatial) model
  
  ## Use model to plot predicted Pr(abundance)
  
  First create a new (aspatial) data set to use with `predict()`
```{r}
Elev <-  seq(
  min(point.data$elev), 
  max(point.data$elev), 
  length = 15)
newdata <- data.frame(elev = Elev)
```

Use model to predict values for different elevations:
  ```{r}
# type = response for predicted probabilities
glm.pred <- predict(
  VATH.elev2, 
  newdata = newdata, 
  type =  "link", 
  se = T) 
```

Logistic regression fits are given in quantiles of the **logistic distribution**, but these numbers aren't very intuitive to interpret.  We can back-transform them onto a probability scale:
```{r}
glm.newdata <-
data.frame(
newdata, 
pred = plogis(glm.pred$fit), 
lcl = plogis(glm.pred$fit + 1.96*glm.pred$se.fit), 
ucl = plogis(glm.pred$fit - 1.96*glm.pred$se.fit))
```

## Plot model coeff. with confidence intervals
```{r}
with(
glm.newdata,
matplot(
x = elev, y = cbind(pred, lcl, ucl), 
type = "l", col = 1, lty = c(1, 2, 2),
xlab = "elevation", ylab = "coeff"),
ylim = c(0, 0.3)
)
  ```
  
  
  ## Map of model predictions
  
  The `raster` package includes an implementation of `predict` that we can use to create a raster layer of model predictions:
  ```{r}
  glm.raster <- predict(
  model = VATH.elev2,
  object = layers,
  type = "response")
  plot(glm.raster, xlab = "Longitude", ylab = "Latitude")
  ```
  
  
  
  # Spatial dependence of model output
  
  
  ## Correlogram of response
  
  Create a correlogram using the authors' function:
    ```{r}
  VATH.cor <- icorrelogram(
    locations = coords, 
    z = point.data$VATH, 
    binsize = 1000, 
    max_dist = 15000)
  VATH.cor
  ```
  
  
  <div class = "red">NOTE: the authors had an error in the `lines()` call below.  They misspecified the upper and lower null envelope column names in the original.</div>
    
    ```{r}
  round(head(VATH.cor, 3), 2)
  plot_ff_correlogram(VATH.cor, "Correlogram of Response", "elevation-polynomial model")
  ```
  
  
  ## Correlogram of model residuals
  
  Build a correlogram with the authors' function:
  ```{r}
  VATH.elev2.res <- residuals(VATH.elev2, type = "deviance")
  corr.res <- icorrelogram(
  locations = coords, 
  z = VATH.elev2.res, 
  binsize = 1000, 
  max_dist = 15000)
  ```
  
  <div class = "red"> NOTE: same comment as above about error in column names.</div>
  ```{r}
  
  plot_ff_correlogram(corr.res, "Correlogram of Residuals", "elevation-polynomial model")
  ```
  
  
  
  contrast results from raw to residuals with mean
  correlogram on residuals of mean model (intercept model)
  ```{r}
  VATH.int <- glm(VATH ~ 1, family = "binomial", data = point.data)
  VATH.int.res <- residuals(VATH.int, type = "deviance")
  corr.int.res <- icorrelogram(locations = coords, z = VATH.int.res, binsize = 1000, max_dist = 15000)
  
  plot_ff_correlogram(corr.int.res, "Correlogram of Residuals", "intercept-only model")
  ```
  
  correlation
  ```{r}
  cor(VATH.cor$Morans.i, corr.int.res$Morans.i)
  ```
  
  
  # Subset data to account for autocorrelation ----
  ```{r}
  #randomly shuffle data by transect and create a shuffled rank vector
  rand.vector <- with(
  point.data, 
  ave(
  POINT, 
  as.factor(TRANSECT), 
  FUN = function(x) {sample(length(x))}))
  
  #pick one random point on transect and remove rest
  point.datasub <- point.data[rand.vector  <=  1, ]
  head(point.datasub, 3)
  
  coords.sub <- cbind(point.datasub$NORTHING, point.datasub$NORTHING)
  head(coords.sub, 3)
  #model
  VATH.sub <- glm(VATH~elev + I(elev^2), family = "binomial", data = point.datasub)
  summary(VATH.sub)

  glmsub.summary <- 
  c(
  summary(VATH.sub)$coef[2, 1], 
  summary(VATH.sub)$coef[2, 2], 
  summary(VATH.sub)$coef[3, 1], 
  summary(VATH.sub)$coef[3, 2])
  
  #correlogram on residuals
  VATH.sub.res <- residuals(VATH.sub, type = "deviance")
  corr.sub.res <- icorrelogram(locations = coords.sub, z = VATH.sub.res, binsize = 2000, max_dist = 15000)
  plot_ff_correlogram(corr.sub.res, "Correlogram of Residuals", "spaced-sample model")
  