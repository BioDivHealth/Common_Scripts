# "Introduction to the NicheMapR microclimate model"

## Getting started: working with the micro_global() function

### Basic operation: modelling microclimates for the average day of each month
#Try running the model for a place of interest to you, as follows:
devtools::install_github('mrke/NicheMapR')
library(NicheMapR) 

longlat <- c(24.9, 60.2) # Helsinki, Finland
# longlat <- c(-89.40123, 43.07305) # Madison, Wisconsin, USA
install.packages('openmeteo')
library(openmeteo)
geocode.out <- openmeteo::geocode('Helsinki')

#longlat <- c(geocode.out$longitude, geocode.out$latitude)
# replace the below with your own file path of choice!!
get.global.climate(folder = '/Users/elisegallois/Desktop/MEB_workshop')
micro <- micro_global(loc = longlat)


head(micro$metout, 2)
head(micro$soil, 2)

require(lattice, quietly = TRUE)
soil <- as.data.frame(micro$soil) # get the soil data
minshade <- micro$minshade[1] # get the value for minimum shade
with(subset(soil, DOY==196 | DOY==349), 
     {xyplot(D0cm + D2.5cm + D5cm + D10cm + D15cm + D20cm + 
               D30cm + D50cm + D100cm + D200cm ~ TIME | 
               as.factor(DOY), ylim = c(-20, 70), 
             xlab = "Time of Day (min)", 
             ylab = "Soil Temperature (deg C)", 
             auto.key = list(columns = 5), 
             as.table = TRUE, type = "b", 
             main = paste(minshade,"% shade"))})  

### Simulating shading by vegetation

micro <- micro_global(loc = longlat, runshade = 0, minshade = 50)
soil <- as.data.frame(micro$soil) # get the soil data
minshade<-micro$minshade[1] # get the value for minimum shade
with(subset(soil, DOY==196 | DOY==349),
     {xyplot(D0cm + D2.5cm + D5cm + D10cm + D15cm + D20cm + 
               D30cm + D50cm + D100cm + D200cm ~ TIME | 
               as.factor(DOY), ylim = c(-20, 70), 
             xlab = "Time of Day (min)", 
             ylab = "Soil Temperature (deg C)", 
             auto.key=list(columns = 5), 
             as.table = TRUE, type = "b", 
             main = paste(minshade, "% shade"))})

### Simulating terrain effects - slope, aspect, hillshade

micro <- micro_global(loc = longlat, runshade = 0, minshade = 0, 
                      slope = 45, aspect = 180)
soil <- as.data.frame(micro$soil) # get the soil data
minshade <- micro$minshade[1] # get the value for minimum shade
with(subset(soil, DOY==196 | DOY==349), 
     {xyplot(D0cm + D2.5cm + D5cm + D10cm + D15cm + D20cm + 
               D30cm + D50cm + D100cm + D200cm ~ TIME | 
               as.factor(DOY), ylim = c(-20, 70), 
             xlab = "Time of Day (min)", 
             ylab = "Soil Temperature (deg C)", 
             auto.key=list(columns = 5), 
             as.table = TRUE, type = "b", 
             main = paste(minshade, "% shade, 45 degree slope, 180 degrees aspect"))})

micro <- micro_global(loc = longlat, runshade = 0, minshade = 0, 
                      hori = c(0, 0, 65, 65, 65, 65, 65, 65, 65, 
                               65, 65, 65, 0, 0, 65, 65, 65, 65, 
                               65, 65, 65, 65, 65, 65))
soil <- as.data.frame(micro$soil) # get the soil data
minshade <- micro$minshade[1] # get the value for minimum shade
with(subset(soil,DOY==196 | DOY==349),
     {xyplot(D0cm + D2.5cm + D5cm + D10cm + D15cm + D20cm + 
               D30cm + D50cm + D100cm + D200cm ~ TIME | 
               as.factor(DOY), ylim = c(-20, 70), 
             xlab = "Time of Day (min)", 
             ylab = "Soil Temperature (deg C)", 
             auto.key = list(columns = 5), 
             as.table = TRUE, type = "b", 
             main=paste(minshade,"% shade, north-south gully"))})

### Changing the soil properties

micro <- micro_global(loc = longlat, runshade = 0, minshade = 0, 
                      soiltype = 0)
soil <- as.data.frame(micro$soil) # get the soil data
minshade <- micro$minshade[1] # get the value for minimum shade
with(subset(soil, DOY==196 | DOY==349), 
     {xyplot(D0cm + D2.5cm + D5cm + D10cm + D15cm + D20cm + 
               D30cm + D50cm + D100cm + D200cm ~ TIME | 
               as.factor(DOY), ylim = c(-20, 70), 
             xlab = "Time of Day (min)", 
             ylab = "Soil Temperature (deg C)",  
             auto.key = list(columns = 5), 
             as.table = TRUE, type = "b", 
             main = paste(minshade, "% shade, rock substrate"))})

### Soil moisture

micro <- micro_global(loc = longlat, runmoist = 1)

head(micro$soilmoist, 2)
head(micro$soilpot, 2)
head(micro$humid, 2)

soilmoist <- as.data.frame(micro$soilmoist) # get the minimum shade soil moisture output
minshade <- micro$minshade[1] # get the value for minimum shade
# append dates
days <- rep(seq(1, 12), 24)
days <- days[order(days)]
dates <- days + soilmoist$TIME / 60 / 24 - 1 # dates for hourly output
soilmoist<-cbind(dates, soilmoist)
for(i in 1:10){
  if(i == 1){
    plot(soilmoist[, i + 3] ~ soilmoist[, 1], 
         ylim = c(0, 0.5), xlab = "Month",  
         ylab = "Soil Moisture (% vol)", 
         col = i, type = "l", 
         main = paste("soil moisture, ", minshade, "% shade"))
  }else{
    points(soilmoist[, i + 3] ~ soilmoist[, 1], 
           col = i, type = "l")
  }
}

# Changing soil hydraulic properties via 'soiltype'

# Rock = 0, sand = 1, loamy sand = 2, sandy loam = 3, 
# loam = 4, silt loam = 5, sandy clay loam = 6, 
# clay loam = 7, silt clay loam = 8, sandy clay = 9, 
# silty clay = 10, clay = 11, user-defined = 12

# PE, Air entry potential (J/kg)
# KS, Saturated conductivity, (kg s/m3) 
# BB, Campbell's soil 'b' parameter (-)
# BD, Soil bulk density (Mg/m3)
# DD, Soil density (Mg/m3)

CampNormTbl9_1 # Campbell and Norman 1990 Table 9.1

# Rerunning with clay as the soil type:

micro <- micro_global(loc = longlat, runmoist = 1, soiltype = 11)

soilmoist <- as.data.frame(micro$soilmoist) # get the minimum shade soil moisture output
minshade <- micro$minshade[1] # get the value for minimum shade
# append dates
soilmoist <- cbind(dates, soilmoist)
for(i in 1:10){
  if(i==1){
    plot(soilmoist[,i+3]~soilmoist[,1], 
         ylim=c(0,0.5), xlab = "Month", 
         ylab = "Soil Moisture (% vol)", col = i,
         type = "l", main=paste("soil moisture, ",minshade,"% shade"))
  }else{
    points(soilmoist[,i+3]~soilmoist[,1] ,col=i,type = "l")
  }
}

### Running at finer time intervals and for longer time periods

# more years
nyears <- 5

micro <- micro_global(loc = longlat, runmoist = 1, soiltype = 11, 
                      nyears = nyears)
soilmoist <- as.data.frame(micro$soilmoist) # get the minimum shade soil moisture output
minshade <- micro$minshade[1] # get the value for minimum shade
# append dates
days <- rep(seq(1, 12 * nyears) / 12, 24)
days <- days[order(days)]
dates <- days + soilmoist$TIME / 60 / 24 / (12) # dates for hourly output
soilmoist <- cbind(dates, soilmoist)
for(i in 1:10){
  if(i==1){
    plot(soilmoist[,i+3]~soilmoist[,1], ylim=c(0,0.5),
         xlab = "Year", ylab = "Soil Moisture (% vol)",
         col=i,type = "l",
         main=paste("soil moisture, ",minshade,"% shade"))
  }else{
    points(soilmoist[,i+3]~soilmoist[,1],col=i,type = "l")
  }
}

# finer time interval

timeinterval <- 365

micro <- micro_global(loc = longlat, runmoist = 1, 
                      soiltype = 11, timeinterval = timeinterval)
soilmoist <- as.data.frame(micro$soilmoist)  # get the minimum shade soil moisture output
minshade <- micro$minshade[1]  # get the value for minimum shade
# append dates
days <- rep(seq(1, timeinterval), 24)
days <- days[order(days)]
dates <- days + soilmoist$TIME/60/24 - 1  # dates for hourly output
soilmoist <- cbind(dates, soilmoist)
for (i in 1:10) {
  if (i == 1) {
    plot(soilmoist[, i + 3] ~ soilmoist[, 1], 
         ylim = c(0, 0.5), xlab = "Day of Year", 
         ylab = "Soil Moisture (% vol)", col = i, type = "l", 
         main = paste("soil moisture, ", minshade, "% shade"))
  } else {
    points(soilmoist[, i + 3] ~ soilmoist[, 1], col = i, 
           type = "l")
  }
}

# plot associated soil temperatures
soil <- as.data.frame(micro$soil)  # get the minimum shade soil temperature output
# append dates
soil <- cbind(dates, soil)
for (i in 1:10) {
  if (i == 1) {
    plot(soil[, i + 3] ~ soil[, 1], ylim = c(-20, 70), 
         xlab = "Day of Year", ylab = "Soil Temperature (deg C)", 
         col = i, type = "l", 
         main = paste("soil temperature, ", minshade, "% shade"))
  } else {
    points(soil[, i + 3] ~ soil[, 1], col = i, type = "l")
  }
}

### Snow

# running two years, first one acting as a 'burn in' year and discarded
timeinterval <- 365
nyears <- 2  
micro <- micro_global(loc = longlat, runmoist = 1, snowmodel = 1, 
                      timeinterval = timeinterval, nyears = 2)
soil <- as.data.frame(micro$soil)[(365 * 24 + 1):(365 * 24 * nyears), ]  # get the minimum shade soil temperature output, discarding the first year
metout <- as.data.frame(micro$metout)[(365 * 24 + 1):(365 * 24 * nyears), ]  # get the minimum shade above ground conditions, discarding the first year
minshade <- micro$minshade[1]  # get the value for minimum shade

# append dates
days <- rep(seq(366, timeinterval * nyears), 24)
days <- days[order(days)]
dates <- days + soil$TIME / 60 / 24 - 1  # dates for hourly output
soil <- cbind(dates, soil)
metout <- cbind(dates, metout)
for (i in 1:10) {
  if (i == 1) {
    plot(soil[, i + 3] ~ soil[, 1], ylim = c(-20, 70), 
         xlab = "Day of Year", 
         ylab = "Soil Temperature (deg C)", 
         col = i, type = "l", 
         main = paste("soil  temperature, ", minshade, "% shade"))
  } else {
    points(soil[, i + 3] ~ soil[, 1], col = i, type = "l")
  }
}
plot(metout$SNOWDEP ~ metout$dates, 
     xlab = "Time of Day (min)", 
     ylab = "snow depth, cm / snow fall, mm", 
     type = "h", 
     main = paste("snow depth (cm) and snow fall (mm) ", minshade, "% shade", sep = ""), 
     col = "light blue")
points(metout$SNOWFALL ~ metout$dates, 
       xlab = "Time of Day (min)", 
       type = "h", col = "blue")

### Plant transpiration and water potential

plant <- as.data.frame(micro$plant)[(365 * 24 + 1):(365 * 24 * nyears), ]  # get the minimum shade plant output, discarding the first year
soilpot <- as.data.frame(micro$soilpot)[(365 * 24 + 1):(365 * 24 * nyears), ]  # get the minimum shade soil water potential output, discarding the first year
plot(plant$TRANS ~ dates, xlab = "Time of Day (min)", ylab = "transpiration, g/h/m^2", type = "h", main = paste("plant transpiration rate ", minshade, "% shade", sep = ""), col = "light blue")
plot(plant$LPT ~ dates, xlab = "Time of Day (min)", ylab = "leaf water potential, J/kg", type = "h", main = paste("plant water potential ", minshade, "% shade", sep = ""), col = "darkgreen")
for (i in 2:10) {
  points(plant[, i + 4] ~ dates, col = i, type = "l")
}

for (i in 2:10) {
  if (i == 2) {
    plot(soilpot[, i + 2] ~ dates, ylim = c(-250, 0), xlab = "Day of Year", ylab = "soil water potential (J/kg)", col = i, type = "l", main = paste("soil water potential ", minshade, "% shade"))
  } else {
    points(soilpot[, i + 2] ~ dates, col = i, type = "l")
  }
}

### Solar spectra - requires microclima package
devtools::install_github("ilyamaclean/microclima")
#library(microclima)
micro <- micro_global(loc = longlat, solonly = 1, 
                      clearsky = 1, lamb = 1, IUV = 1) 
#microclima = 1) # optional - invoke microclima

metout <- as.data.frame(micro$metout)
drlam <- as.data.frame(micro$drlam) # direct solar
srlam <- as.data.frame(micro$srlam) # scattered solar
drrlam <- as.data.frame(micro$drrlam) # direct Rayleigh solar, excludes the effects of water vapor, clouds, and aerosols
diffuse_frac <- micro$diffuse_frac

par(mfrow = c(1, 1))
# solstice (mid December, 11 * 24 + 13)
lambdas <- as.numeric(colnames(drlam[3:113])) # get wavelengths
plot(lambdas, drrlam[277, 3:113], type = 'l', ylab = 'irradiance, W/m2', xlab = 'wavelength, nm', ylim = c(0, 1.8), main = 'solar spectra, mid December')
points(lambdas, drlam[277, 3:113], type = 'l', col = 2)
points(lambdas, srlam[277, 3:113], type = 'l', col = 3)

# equinox (mid March, 2 * 24 + 13)
plot(lambdas, drrlam[61, 3:113], type = 'l', ylab = 'irradiance, W/m2', xlab = 'wavelength, nm', ylim = c(0, 1.8), main = 'solar spectra, mid March')
points(lambdas, drlam[61, 3:113], type = 'l', col = 2)
points(lambdas, srlam[61, 3:113], type = 'l', col = 3)

# solstice (mid June, 5 * 24 + 13)
plot(lambdas, drrlam[133, 3:113], type = 'l', ylab = 'irradiance, W/m2', xlab = 'wavelength, nm', ylim = c(0, 1.8), main = 'solar spectra, mid June')
points(lambdas, drlam[133, 3:113], type = 'l', col = 2)
points(lambdas, srlam[133, 3:113], type = 'l', col = 3)

library(pracma) # Practical Numerical Math Functions
row2calc <- 2 * 24 + 13 # mid March
# integrate across wavelengths with trapezoid method
SOLR_dir <- trapz(lambdas, t(drlam[row2calc, 3:113]))
SOLR_dif <- trapz(lambdas, t(srlam[row2calc, 3:113]))

metout$SOLR[row2calc] # horizontal plane total solar, W/m^2
SOLR_dir + SOLR_dif # sum the direct and diffuse integrations
diffuse_frac[row2calc] # microclima predicted diffuse fraction
SOLR_dif / (SOLR_dir + SOLR_dif) # NMR predicted diffuse fraction

# compare across all hours
SOLR_difs <- rep(NA, nrow(metout))
SOLR_dirs <- SOLR_difs

for(i in 1:nrow(metout)){
  row2calc <- i
  SOLR_dirs[i] <- trapz(lambdas, t(drlam[row2calc, 3:113]))
  SOLR_difs[i] <- trapz(lambdas, t(srlam[row2calc, 3:113]))
}
plot(micro$dates, SOLR_difs / (SOLR_dirs + SOLR_difs), type = 'l', ylab = 'diffuse fraction', xlab = 'month')
points(micro$dates, diffuse_frac, type = 'l', col = 2)
