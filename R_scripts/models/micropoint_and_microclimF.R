# =========================================================================== #
# ~~~~~~~~~~~~~~~~~~~~ micropoint package examples ~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# =========================================================================== #
# ~~~~~~~~ Install package ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
require(devtools)
install_github("ilyamaclean/micropoint")
install_github("ilyamaclean/microclimf")

# any issues installing, first check package dependancies installl OK
require(stats)
require(Rcpp)
# ~~~~~~~~ inspect and get details of model inputs ~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# These are the datasets included with the package
library(micropoint)
library(microclimf)
head(climdata)
?groundparams
?forestparams
# Lots of unintuitive soil physical and hydraulic parameters. Lets see how 
# these can be generated more easily:
mygroundp <- creategroundp("Loam")
mygroundp # default is to make ground flat
# Function uses inbuilt look-up table (which also gives available soil types)
soilparams
# Some vegetation parameters also a bit unintuative, but you won't go far wrong
# by leaving them as their defaults. Thus easy enough to swap in or out, those
# you know. E.g.:
myvegparams <- forestparams
myvegparams$h <- 20 # change height to 20 m
# Let us see how the skew and spread parameter is used to generate a plausable
# height profile of plant area index (PAI) values for each height (not you can supply 
# your own vector of these in which case the skew and spread parameter are ignored)
paii <- PAIgeometry(PAI = 5, skew = 7, spread = 70, n = 1000)
z <- c(1:1000) / 100
# plant area within each layer
plot(z ~ paii, type = "l", main = paste("Total PAI:", sum(paii)))
# foliage density
fd <- paii * 1000 / 10 # 1000 layers, 10 m tall
plot(z ~ fd, type = "l", main = paste("Total PAI:", mean(fd) * 10))
# ~~~~~~~~ Run the model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Lets run the point model using inbiuilt parameters for one metre above ground
# skew and spread used instead of paii vector. 
mout <- micropoint::runpointmodel(climdata, reqhgt = 1, forestparams, paii = NA,  
                                  groundparams, lat =49.96807, long= -5.215668, n = 20) # 20-layer canopy model
# Ask Ilya about the warnings given. These are deliberate to alert you to a common issue
tme <- as.POSIXct(climdata$obs_time)
plot(mout$tair ~ tme, type="l", xlab = "Month", ylab = "Air temperature",
     ylim = c(-5, 30), col = "red")   # microclimate
par(new = TRUE)
plot(climdata$temp ~ tme, type="l", xlab = "", ylab = "",
     ylim = c(-5, 30), col = rgb(0, 0, 0, 0.5))  # macroclimate
# Let is instead plot a vertical height profile for the hottest hour
# plot air temperature for hottest hour
xx <- plotprofile(climdata, hr = 4094, "tair", forestparams, paii = NA, 
                  groundparams, lat = 49.96807, long= -5.215668)
# plot leaf temperature for hottest hour
xx<- plotprofile(climdata, hr = 4094, "tleaf", forestparams, paii = NA, 
                 groundparams, lat = 49.96807, long= -5.215668)
# =========================================================================== #
# ~~~~~~~~~~~~~~~~~~~~ microclimf package examples ~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# =========================================================================== #
# install package
detach(package:micropoint, unload = TRUE)  # remove micropoint to avoid name conflicts
#devtools::install_github("ilyamaclean/microclimf")
library(microclimf)
library(terra)
# Runs point microclimate model built into microclimf with inbuilt datasets (takes ~ 5 seconds)
micropoint <- runpointmodel(climdata, reqhgt = 0.05, dtmcaerth, vegp, soilc)
# Subset point model outputs (here gets all hours in warmest day of each month)
micropoint <- subsetpointmodel(micropoint, tstep = "month", what = "tmax")
head(micropoint$weather) # inspect which data have been retained
# Run model 5 cm above ground with subset values and inbuilt datasets (takes ~10 seconds)
# you may wish to inspect the model inputs before doing so. These are stored here
# as terra::PackedSpatRasters. Just use the rast function to unpack them. microclimf
# autochecks whether or not PackedSpatRasters are supplied). E.g.
plot(rast(dtmcaerth))

# Note here - monthly valued are supplied. Ask Ilya about this 
mout <- runmicro(micropoint, reqhgt = 0.05, vegp, soilc, dtmcaerth)
attributes(mout)

# Plot air temperatures on hottest hour in 
# micropoint (2017-06-20 13:00:00 UTC)
library(terra)
mypal <- colorRampPalette(c("darkblue", "blue", "green", "yellow", 
                            "orange",  "red"))(255)
plot(rast(mout$Tz[,,134]), col = mypal, range = c(20, 48))
# Note that darker areas correspond to shaded areas:
plot(rast(vegp$pai)[[5]])






library(micropoint) 
head(climdata)

?groundparams
mygroundp<-creategroundp("Loam") 
?
  
  
  ?forestparams


pai <- PAIgeometry(5, skew = 7, spread = 70, 
                   n = 1000)
z <- c(1:1000) / 100
plot(z ~ pai, type = "l", 
     main = paste("Total PAI:", sum(pai)))


