# install.packages("rgeos")


# Setup ----
{
  source("data/environment_vars.R")
  dir.exists(book_data)
  
  require(raster)
  require(rgdal)
  require(rgeos)
}

# 2.3.4.1 ----

# Read data files ----
{
  
  nlcd = raster(paste0(book_data, "nlcd2011SE"))
  proj4string(nlcd)
  res(nlcd)
  
  # Study site data ----
  sites = readOGR(paste0(book_data, "reptiledata"))
  sites
  head(sites)
  
  # there is no projection info!
  # Normally we should panic, but we can acertain from the book's 
  # instructions that it is in the same projection as tne land cover.
  proj4string(sites) = proj4string(nlcd)
}

# Remove all corn land use ----
{
  # The book uses:
  sites$management
  sites_1 = subset(sites, management != "Corn")
  
  # One caveat is that this won't catch data that were coded as 'corn' or 'CORN' or even ' corn  ' (with spaces)
  # From experience, it is easy to find inconsistencies in the way data are coded.
  # I would probably want to check that I wasn't missing anything.
  
  # You could first change all of the management entries to lowercase:
  grep("corn", tolower(sites$management))
  
  # You could also use the ignore.case in grep.  I would probably use this option.
  grep("corn", sites$management, ignore.case = TRUE)
  
  # We need the management types that do not use corn.
  # We can invert the grep output to return elements that do not
  # match
  grep("corn", sites$management, ignore.case = TRUE, invert = TRUE)
  
  # Do we get the same results with subset() and grep()?
  nrow(subset(sites, management != "Corn"))
  length(grep("corn", sites$management, ignore.case = TRUE, invert = TRUE))
  
  # We could do all of this inside subset using grepl()
  sites_1 = subset(sites, !grepl("corn", sites$management, ignore.case = TRUE))
  
  # There could still be problems though!
  # grep() and grepl() try to match a pattern anywhere in the text.
  # What if we had a management type called "not corn"?
  
  # What would you do to make sure you weren't missing incorrectly or inconsistently recorded data?
}

{
  # crop raster to study site ----
  # It's faster to do the crop before the factoring.
  
  # There are many ways to do the cropping
  
  # F&F create an intermediate extent
  extent.new = 
    extent(c(
      xmin = min(sites$coords_x1) - 10000,
      xmax = max(sites$coords_x1) + 10000,
      ymin = min(sites$coords_x2) - 10000,
      xmax = max(sites$coords_x2) + 10000
    ))
  
  # nlcd_2 = crop(nlcd, extent.new)
  
  # Their method requires us to manually set new min and max coords for the bounding box
  # It's much easier, and less error prone, to buffer the extent of the sites data
  # We get the same result with more compact and elegant code:
  extent(buffer(as(extent(sites), "SpatialPolygons"), 1e4)) == extent.new
  
  # We can do the crop without adding an intermediate variable 'extent.new' to our workspace:
  nlcd_2 = crop(
    nlcd, 
    extent(buffer(as(extent(sites), "SpatialPolygons"), 1e4))
  )
  nlcd_2
  
  
  # Factor the land cover data ----
  # Now do the factoring
  nlcd_f = as.factor(nlcd_2)
  
  # Reclassify land cover to forest/non-forest
  levels(nlcd_f[])
  nlcd_2
  nlcd_f
  
  
  # The text tells us that categories 41, 42, and 43 are forest
  levels(nlcd_f)
  
  # We can use a handy operator to tell us which levels match 41, 42, or 43
  levels(nlcd_f)[[1]][, 1] %in% 41:43
  
  # Coerce boolean to integer
  cbind(
    ID = levels(nlcd_f)[[1]],
    reclass = as.integer(levels(nlcd_f)[[1]][, 1] %in% 41:43)
  )
  
  # trick to implicitly coerce: add 0 to boolean to get an integer
  cbind(
    ID = levels(nlcd_f)[[1]],
    reclass = 0 + levels(nlcd_f)[[1]][, 1] %in% 41:43
  )
  
  # This trickery only works because we were reclassifying as 0 or 1.
  forest = reclassify(
    nlcd_f, 
    cbind(
      ID = levels(nlcd_f)[[1]],
      reclass = 0 + levels(nlcd_f)[[1]][, 1] %in% 41:43
    )
  )
}

plot(forest)
plot(sites_1, add = T)
plot(sites, add = T, col = 2)


# Illustration with site 1 ----
{
  b1 = 1e3
  b2 = 5e3
  
  bf1 = buffer(sites[1, ], width = b1)
  bf2 = buffer(sites[1, ], width = b2)
  plot(0, type = "n")
  zoom(nlcd, bf1)
  zoom(nlcd, bf1, legend = F, axes = F, col = terrain.colors(5))
  zoom(nlcd, bf2, legend = F, axes = F, col = terrain.colors(5))
  
  
  plot(bf1, add = T)
  points(sites, pch = 19, cex = 2,  add = TRUE)
  
  # dev.off() is your friend
  dev.off()
  
  # crop is fast, mask is slow.
  # mask keeps the original raster extent.
  # if you crop first, you only have to mask a small raster
  
  # mask is much slower than crop:
  t1 = proc.time()
  m1 = mask(forest, bf1)
  elapsed = proc.time() - t1
  elapsed
  
  t1 = proc.time()
  m2 = crop(forest, bf1)
  elapsed = proc.time() - t1
  elapsed
  
  plot(crop(m1, bf1))
  plot(m2)
  
  plot(mask(crop(forest, bf1), bf1))
  plot(mask(crop(forest, bf2), bf2))
  
  
  # crop and mask code on page 37 won't do what you want...
  
}


polys = sites
i = 1
buff_width = 1e3
landcover = forest

BufferCover = 
  function(polys, i, buff_width, landcover)
  {
    hectare_to_sq_meters = (100 * 100)
    
    # conversion factor for square meters to hectares
    conv = 1 / 1e4
    
    grain     = (res(landcover)[[1]] ^ 2) * conv
    buff_area = pi * (buff_width ^ 2) * conv
    coords_i  = SpatialPoints(cbind(polys[i, 1]))
    buff_i    = gBuffer(coords_i, width = buff_width)
    crop_i    = crop(landcover, buff_i)
    crop_NA   = setValues(crop_i, NA) # needed for rasterize()
    buff_rst  = rasterize(buff_i, crop_NA) # make a raster from the buffer
    land_buf  = mask(x = crop_i, mask = buff_rst)
    n_cells   = cellStats(land_buf, "sum")
    cover_tot = cellStats(land_buf, "sum") * grain
    cover_pct = 100 * (cover_tot / buff_area)
    return(cover_pct)
  }
BufferCover(sites, 1, 1e3, forest)

plot.raster



sites_1
sites

plot(buffer(as(extent(sites), "SpatialPolygons"), 1000))
plot(buffer(as(extent(sites), "SpatialPolygons"), -10000), add = T)


as(extent(sites), "SpatialPolygons")
buffer(as(extent(sites), "SpatialPolygons"), -10000)
min(sites$coords_x1) - 1e4

extent(sites) - 1000

to_crop = 1e4
c(bbox(sites)) + c(-to_crop, to_crop)

ext2 = (extent(sites)) + c(-to_crop, to_crop)

min(sites$coords_x1) - 10000

plot(sites)
plot(nlcd, add = T)

extent(nlcd)
extent(sites)
proj4string(sites) = proj4string(nlcd)
sites

plot(nlcd)
plot(sites, add = T)



nlcd_2 = crop(nlcd, extent(gBuffer(as(extent(sites), "SpatialPolygons"), width = +10000)))
ncell(nlcd)
ncell(nlcd_2)

# Land cover raster is coded as integers. We need it to be a factor.
nlcd_f = as.factor(nlcd)



bbox(sites)

extent(sites)
buffer(as(extent(sites), SpatialPolygonsDataFrame), 100)
