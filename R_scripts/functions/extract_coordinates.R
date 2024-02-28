## GPS coordinate extraction


# For this script to work, you first need to create a project in Google cloud platform
# You then need to generate an API key from this project
# Use Chat GPT to give you a step-by-step on how to set up the project and create the API key


# Libraries
{library(pacman)
p_load(ggmap, sf, tidyverse)}


# Load data with locations names
df <- read.csv("location_data.csv")



# API ----



# Paste API generated from Google Cloud Platform to create character vector
my_gmap_api <- "paste_key_here"


# Double check API key is registered
register_google(key = my_gmap_api)




# Test extraction ----



# Specify location you want coordinates for
location <- "natural history museum, london"


# Extract and store GPS coordinates into object
coordinates <- geocode(location)


# Print coordinates
options(digits = 5)
print(coordinates)


# Source the area you're interested in using google base map 
region_oi <- get_map(location = "london", 
                     maptype = "roadmap", # can change to "satellite"
                     source = "google", 
                     api_key = my_gmap_api)


# Plot map
ggmap(region_oi)


# Plot coordinates as a point on map 
ggmap(region_oi) +
 geom_point(data = coordinates,
            aes(x = lon, y = lat),
            colour = "red",
            size = 2)


# Extra code to add arrow
 # geom_segment(aes(x=37, y=-1, 
 #                  xend=36.82195, yend=-1.292066), 
 #              arrow = arrow(length=unit(.5, 'cm')),
 #              colour = "red", lwd = 1)



# Extract coordinates for location name variable  ----



# Dummy data
df <- data.frame(location = c("soho, london", 
                              "angel, london", 
                              "vauxhall, london", 
                              "paddington, london", 
                              "shoreditch, uk"))


# Use the geocode() function from ggmap to extract coordinates for location names
df <- df %>% mutate(geocode(location))



# Create base map to check extracted coordinates
check_coords <- get_map(location = "london", 
                     maptype = "roadmap", # can change to "satellite"
                     source = "google", 
                     zoom = 12,
                     api_key = my_gmap_api) 


# Plot coordinates on base map
ggmap(check_coords) +
  geom_point(data = df,
             aes(x = lon, y = lat),
             colour = "red",
             size = 3)



# End ----
