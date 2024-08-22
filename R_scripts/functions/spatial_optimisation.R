# This is an example for West Africa but you can alter the countries as you see fit
# general dependencies 
library(data.table); library(tidyr); library(dplyr); library(reticulate); library(sf); library(ggplot2); library(magrittr); library(purrr);
library(lubridate); library(spdep); library(Hmisc); library(raster); library(rnaturalearth); library(rnaturalearthdata); library(terra);
library(osmdata); library(rgdal); library(worldpop); library(ggthemes)

# 1 -  West Africa polygons and raster files -----

# Get shapefile for African countries
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Define West African countries
west_africa_countries <- c("Senegal", "Gambia", "Guinea-Bissau", "Guinea", "Sierra Leone", 
                           "Liberia", "Burkina Faso", "Côte d'Ivoire", "Ghana", "Nigeria",
                           "Togo", "Benin", "Mali",
                           "Central African Republic",
                           "Republic of the Congo", "Democratic Republic of the Congo")

# Filter out West African countries
shp <- africa[africa$name %in% west_africa_countries, ]

# Plot the shapefile
plot(shp)

# Get West Africa travel times
# download from malariaAtlas project
ff = malariaAtlas::getRaster(
surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
shp = as_Spatial(shp))

# chop off Northern Mali - you can adjust this as you see fit, I just needed to chop off an area above a certain latitude
lat_threshold <- 17
ext <- extent(ff)

# Set the new extent to crop based on latitude threshold
new_ext <- extent(ext@xmin, ext@xmax, ext@ymin, lat_threshold)

# Crop the raster based on the new extent
ff_cropped <- crop(ff, new_ext)

# Save the travel time raster
output_file <- "data/west_africa_friction.tif"
writeRaster(ff_cropped, filename = output_file, format = "GTiff", options = "COMPRESS=DEFLATE")

# load the friction surface raster - this is for when you've already generated it
ff <- raster(paste0("data/friction_surfaces/west_africa_friction.tif")) 
plot(ff)

# Download population data from WorldPop
# Set the directory where your raster files are stored
raster_dir <- "/YOUR_FILEPATH/tot_pop_tif"   

# List all tif files in the directory
raster_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)

# Read the raster files
raster_list <- lapply(raster_files, raster)

# Mosaic the rasters
mosaic_raster <- do.call(mosaic, c(raster_list, fun = mean))

# chop off Northern Mali - again this is just an example fitting this particular case
lat_threshold <- 17 
ext <- extent(mosaic_raster)

# Set the new extent to crop based on latitude threshold
new_ext <- extent(ext@xmin, ext@xmax, ext@ymin, lat_threshold)

# Crop the raster based on the new extent
mosaic_raster <- crop(mosaic_raster, new_ext)

# Save the mosaic raster
output_file <- "data/population_waf/total_pop_merged.tif"
writeRaster(mosaic_raster, filename = output_file, format = "GTiff", 
            overwrite = TRUE, options = "COMPRESS=DEFLATE")

# Load the mosaic raster
mosaic_raster <- raster(paste0("data/population_waf/total_pop_merged.tif")) 


# Aggregate rasters for easier compute
pop_ras <- aggregate(mosaic_raster, fact=100, fun=mean)
output_file <- "data/population_waf/aggregated_pop_merged.tif"
writeRaster(pop_ras, filename = output_file, format = "GTiff", 
            overwrite = TRUE, options = "COMPRESS=DEFLATE")

# load aggregated population raster
pop_ras <- raster(paste0("data/population_waf/aggregated_pop_merged.tif")) 

# clip ff to match pop extent
clipped_ff <- crop(ff, pop_ras)
plot(clipped_ff)

# save clipped file
output_file <- "data/friction_surfaces/west_africa_ff_clipped.tif"
writeRaster(clipped_ff, filename = output_file, format = "GTiff", 
            overwrite = TRUE, options = "COMPRESS=DEFLATE")

# Load the clipped friction surfaces raster
clipped_ff <- raster(paste0("data/friction_surfaces/west_africa_ff_clipped.tif")) 


# Define the bounding box based on the raster
raster_extent <- ext(pop_ras)
bbox <- st_bbox(c(xmin = xmin(raster_extent), xmax = xmax(raster_extent),
                  ymin = ymin(raster_extent), ymax = ymax(raster_extent)),
                crs = st_crs(4326))  # WGS84 coordinate system

# Query for cities within the bounding box
city_query <- opq(bbox) %>%
  add_osm_feature(key = "place", value = "city") %>%
  osmdata_sf()

# Extract points from the query results
cities <- city_query$osm_points
# Ensure cities data has the required columns and filter if necessary
cities <- cities %>% 
  st_as_sf() %>%
  dplyr::filter(!is.na(place)) %>%
  dplyr::mutate(x = st_coordinates(.)[,1],
                y = st_coordinates(.)[,2])

write.csv(cities, "data/west_africa_cities.csv")
cities <- read.csv("data/west_africa_cities.csv")

# Aggregate tmat raster
# clipped_ff <- raster(clipped_ff)
clipped_ff_aggregated <- aggregate(clipped_ff, fact=10, fun=max) # CHANGE WHEN RUN THRU ONCE
plot(clipped_ff_aggregated)
#clipped_ff_aggregated <- raster(clipped_ff_aggregated)

#create movement matrix and apply geocorrection
# https://medium.com/@abertozz/mapping-travel-times-with-malariaatlas-and-friction-surfaces-f4960f584f08
tmat = gdistance::transition(clipped_ff_aggregated, function(x) 1/mean(x), 8) 
tmat_gc = gdistance::geoCorrection(tmat)

# Number of vaccine stockpile locations to test
number_points <- 1:15

# Dataframe for saving result
result <- data.frame()


# 2a - Run friction surface only optimisation ----
# Extract values at point locations and drop NA values
city_values <- extract(pop_ras, cities)
cities <- cbind(cities, pop_value = city_values)
cities <- cities %>% filter(!is.na(pop_value))
plot(pop_ras)
points(cities)

for (n in number_points) {
  print(paste("Optimising for", n, "locations", sep = " "))
  
  # Use the cities as potential locations
  pt <- cities %>%
    st_drop_geometry() %>%  # Remove geometry column
    dplyr::select(x, y)      # Retain only "x" and "y" columns"
  
  # For result
  res_n <- data.frame()
  
  # Select rep sets of n points (here 10 repeats for example)
  for (rep in 1:20) {
    cat("Repeat:", rep, "\n")
    
    # Sample n points
    ptx <- pt[sample(1:nrow(pt), n, replace = FALSE), ]
    
    # Ensure ptx is correctly formatted
    from_coords <- as.matrix(ptx)
    
    # Calculate least cost distance with Inf check
    lcd_rep <- gdistance::costDistance(tmat_gc, from = from_coords, to = coordinates(pop_ras))
    
    # Check for all Inf and handle appropriately (replace with NA or large value)
    if (all(is.infinite(lcd_rep))) {
      # Handle all Inf case (consider replacing with NA or large value)
      cat("Warning: All distances are infinite. Consider adjusting sampling or cost distance function.\n")
      lcd_rep[lcd_rep == Inf] <- NA  # Replace with NA (example)
    } else {
      # If not all Inf, replace individual Inf with NA
      lcd_rep[lcd_rep == Inf] <- NA
    }
    
    # Calculate closest distances with NA handling
    closest <- apply(lcd_rep, 2, min, na.rm = TRUE)
    closest[closest == Inf] <- NA  # Ensure no remaining Inf
    
    # Check for all NA in closest and handle (might indicate unreachable locations)
    if (all(is.na(closest))) {
      # Handle all NA case (consider adjusting sampling or cost distance function)
      cat("Warning: All closest distances are NA. Consider adjusting sampling or cost distance function.\n")
      # You can skip calculating meantt or set a specific value here (e.g., meantt <- NA)
    } else {
      # Calculate mean of closest distances (excluding NA)
      meantt <- mean(closest, na.rm = TRUE)
    }
    
    # Save the result
    ptx$rep <- rep
    ptx$mean_tt <- meantt
    ptx$n_locations <- n
    res_n <- rbind(res_n, ptx)
  }
  
  # Extract optimal rep
  opt <- res_n %>%
    dplyr::arrange(mean_tt)
  
  
  result <- rbind(result, opt)
}

# save results
write.csv(result, "output/optimiser/westafrica_popweight.csv", row.names=FALSE)
result <- read.csv("output/optimiser/westafrica_popweight.csv")


# 2b - Border agnostic popn weighting strategy ----

# Initialize result data frame
result <- data.frame()

for (n in number_points) {
  print(paste("Optimising for", n, "locations", sep = " "))
  
  # Use the cities as potential locations
  pt <- cities %>%
    st_drop_geometry() %>%  # Remove geometry column
    dplyr::select(x, y)      # Retain only "x" and "y" columns
  
  # Extract raster_incidence values for points in pt
  pt_coords <- pt[, c("x", "y")]
  pt$pop <- raster::extract(pop_ras, pt_coords)
  pt$pop <- pt$pop / sum(pt$pop)
  
  # Initialize result dataframe for current n
  res_n <- data.frame()
  
  # Select rep sets of n points (here 6 repeats for example)
  for (rep in 1:20) {
    cat("Repeat:", rep, "\n")
    
    # Sample n points
    ptx <- pt %>%
      sample_n(size = n, replace = FALSE, prob = pt$pop)
    
    # Ensure ptx is correctly formatted
    from_coords <- as.matrix(ptx[, c("x", "y")])
    
    # Calculate least cost distance with Inf check
    lcd_rep <- gdistance::costDistance(tmat_gc, from = from_coords, to = coordinates(pop_ras))
    
    # Check for all Inf and handle appropriately (replace with NA or large value)
    if (all(is.infinite(lcd_rep))) {
      # Handle all Inf case (consider replacing with NA or large value)
      cat("Warning: All distances are infinite. Consider adjusting sampling or cost distance function.\n")
      lcd_rep[lcd_rep == Inf] <- NA  # Replace with NA (example)
    } else {
      # If not all Inf, replace individual Inf with NA
      lcd_rep[lcd_rep == Inf] <- NA
    }
    
    # Calculate closest distances with NA handling
    closest <- apply(lcd_rep, 2, min, na.rm = TRUE)
    closest[closest == Inf] <- NA  # Ensure no remaining Inf
    
    # Check for all NA in closest and handle (might indicate unreachable locations)
    if (all(is.na(closest))) {
      # Handle all NA case (consider adjusting sampling or cost distance function)
      cat("Warning: All closest distances are NA. Consider adjusting sampling or cost distance function.\n")
      meantt <- NA  # Set meantt to NA
    } else {
      # Calculate mean of closest distances (excluding NA)
      meantt <- mean(closest, na.rm = TRUE)
    }
    
    # Save the result
    ptx$rep <- rep
    ptx$mean_tt <- meantt
    ptx$n_locations <- n
    
    # Ensure the column structure of ptx matches res_n
    if (nrow(res_n) == 0) {
      res_n <- ptx
    } else {
      res_n <- rbind(res_n, ptx)
    }
  }
  
  # Extract optimal rep
  opt <- res_n %>%
    dplyr::arrange(mean_tt)
  
  # Ensure the column structure of opt matches result
  if (nrow(result) == 0) {
    result <- opt
  } else {
    result <- rbind(result, opt)
  }
}

# Rename columns in nigeria_cities to match the 'result' data frame
wa_cities <- subset(world.cities, country.etc = c("Senegal", "Gambia", "Guinea-Bissau", "Guinea", "Sierra Leone", 
                                                  "Liberia", "Burkina Faso", "Côte d'Ivoire", "Ghana", "Nigeria",
                                                  "Togo", "Benin", "Mali",
                                                  "Central African Republic",
                                                  "Republic of the Congo", "Democratic Republic of the Congo")
                    , select = c(name, lat, long)) %>%
  rename(x = long, y = lat)

# Function to find the nearest city
find_nearest_city <- function(stockpile_coords, cities) {
  # Check and ensure stockpile_coords is a numeric vector with 2 elements (lon, lat)
  if (length(stockpile_coords) != 2 || !is.numeric(stockpile_coords)) {
    stop("stockpile_coords must be a numeric vector of length 2 (longitude, latitude)")
  }
  
  # Check the range of longitude and latitude
  if (stockpile_coords[1] < -180 || stockpile_coords[1] > 180) {
    stop("Longitude must be between -180 and 180 degrees.")
  }
  if (stockpile_coords[2] < -90 || stockpile_coords[2] > 90) {
    stop("Latitude must be between -90 and 90 degrees.")
  }
  
  # Calculate the distances from the stockpile point to all cities
  distances <- distHaversine(matrix(stockpile_coords, ncol = 2), cities[, c("x", "y")])
  nearest_city_index <- which.min(distances)
  return(cities[nearest_city_index, "name"])
}

# Apply the function to each row of 'result' to find the nearest city
result$city_name <- apply(result, 1, function(row) {
  stockpile_coords <- as.numeric(c(row['x'], row['y']))
  print(paste("Processing coordinates:", stockpile_coords[1], stockpile_coords[2]))
  find_nearest_city(stockpile_coords, wa_cities)
})

# save results
write.csv(result, "output/optimiser/westafrica_borderagnostic_popweight.csv", row.names=FALSE)
result <- read.csv("output/optimiser/westafrica_borderagnostic_popweight.csv")

