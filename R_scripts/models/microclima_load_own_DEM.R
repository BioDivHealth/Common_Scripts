### Elise Gallois, elise.gallois94@gmail.com ###
### Date Created: 5th August 2020 ###

#set working directory
rm(list=ls(all=TRUE))

#note to user: set to the filepath you have saved the DSM file onto (aka, where the TIF tile is stored)
#setwd("________")

#load libraries
install.packages("devtools")
devtools::install_github("ilyamaclean/microclima")
devtools::install_github('mrke/NicheMapR')
"rgdal_show_exportToProj4_warnings"="none"
library(devtools)
library(sp)
library(rgdal)
library(raster)
library(ggplot2) 
library(viridis)
library(rasterVis)
library(microclima)
library(elevatr)
library(NicheMapR)
library(RNCEP)
library(insol) 

# 1a. PREPARE DSM RASTER LAYER ----
# load topography raster data
# NOTE: save to 'spatial' folder in data folder
qhi_crop <- raster('data/spatial/qhi_crop.tiff') # insert your tif here, this one included as an example

# quick plot DSM raster 
plot(qhi)

#### 1b - VISIBILITY 1: cropped original very large DSM using points shapefile ----
## DO NOT NEED TO RUN if you have already downloaded the cropped file ##

# import GPS points to crop raster, insert your own shapefile here
#crop_extent <- readOGR("tea_pointsCRS.shp")
#proj4string(crop_extent)
#crs(crop_extent) <- '+proj=utm +zone=7 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'

# crop the lidar raster using the vector extent
#qhi_crop <- crop(qhi, crop_extent)
#plot(qhi_crop, main = "Cropped DSM")


# 2. RUN MODEL ----
#download global climates dataset from nichemapr (only run if not downloaded onto computer! huge file)
get.global.climate()

# prepare raster at 1m res 
# note that you can supply coords for the model to download DEM from their own source, but best to 
# use your own if you have one!
r <- microclima::get_dem(qhi_crop, resolution = 1)

# Modelling: takes > 30 minutes to run! 
# Set your date range, height above ground, habitat type etc. Run ??runauto for more info
temps <- microclima::runauto(r, "09/08/2017", "09/08/2017", hgt = 0.1,
                             l = NA, x = NA, habitat = "Open shrublands", 
                             plot.progress = TRUE)


#extract mean temp outputs
meantemp <- temps$tmean

#plot mean min and max temp outputs 
plot(meantemp, main = "Mean Temperature (\u00B0C)", col = inferno(100))

#save mean min and max temp as rasters
writeRaster(meantemp, 'data/spatial/thermsum/09a.tif', format = 'GTiff')


# 3. OPTIONAL: ADD ALL NEW MODELS TOGETHER TO MAKE THERMAL SUM ----
files = list.files("data/spatial/thermsum",pattern="*.tif$", full.names=TRUE)
rs <- stack(files)
thermalsum <- calc(rs, sum)

# plot thermal sum temp outputs 
plot(thermalsum, main = "28-day Thermal Sum (\u00B0C)", col = inferno(100))

# save thermal sum temp temp as rasters
writeRaster(thermalsum, 'data/spatial/microclima/thermsum.tif', format = 'GTiff', overwrite = TRUE)


# 4. OPTIONAL: EXTRACT SLOPE, ELEVATION AND ASPECT FROM DEM ----

# reaggregate resolution from 10cm to 1m
res(qhi_crop) # check resolution = 10x10cm
qhi_1m <- aggregate(qhi_crop, fact=10) # multiply by factor of 10 to scale up appropriately
res(qhi_1m) # check resolution = 1x1m

# write new elevation raster
writeRaster(qhi_1m, 'data/spatial/elevation.tif', format = 'GTiff', overwrite = TRUE)

# tif of slope
slope <- terrain(qhi_1m, opt = 'slope', unit = 'degrees')  #calculate slope
# write new slope raster
writeRaster(slope, 'data/spatial/slope.tif', format = 'GTiff', overwrite = TRUE)

# tif of aspect
aspect <- terrain(qhi_1m, opt = 'aspect', unit = 'degrees') #calculate aspect
# write new slope raster
writeRaster(aspect, 'data/spatial/aspect.tif', format = 'GTiff', overwrite = TRUE)
