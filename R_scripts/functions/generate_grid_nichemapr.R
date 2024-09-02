library(NicheMapR)
library(microclima)
library(terra)

# specify your site
loc <- c(11.09999995691138, 47.033624786779995)

# get a DEM for the site
dem <- get_dem(lat = loc[2], long = loc[1], xdims = 20, ydims = 20)
plot(dem)
slope.grid <- terrain(dem, v="slope")
aspect.grid <- terrain(dem, v="aspect")
plot(slope.grid)
plot(aspect.grid)

# create a grid of points (lat and long) to simulate
extent <- ext(dem)
utm_x <- seq(extent[1], extent[2], by = res(dem)[1])
utm_y <- seq(extent[3], extent[4], by = res(dem)[2])
grid_utm <- expand.grid(x = utm_x, y = utm_y)
points_utm <- vect(grid_utm, geom=c("x", "y"), crs = crs(dem))
crs_latlong <- "+proj=longlat +datum=WGS84"
points_latlong <- project(points_utm, crs_latlong)
latlong_coords <- crds(points_latlong)
grid_lonlat <- data.frame(lon = latlong_coords[,1], lat = latlong_coords[,2])

# extract slopes and aspects
slopes <- extract(slope.grid, points_utm)[, 2]
slopes[is.na(slopes)] <- 0
aspects <- extract(aspect.grid, points_utm)[, 2]
aspects[is.na(aspects)] <- 0
elevs <- extract(dem, points_utm)[, 2]

# slow approach
# max.5cm.soils <- matrix(nrow = nrow(grid_lonlat), ncol = 1)
# for(i in 1:nrow(grid_lonlat)){
#   micro <- micro_global(loc = c(grid_lonlat[i, 1], grid_lonlat[i, 2]),
#                         slope = slopes[i],
#                         aspect = aspects[i],
#                         elev = elevs[i],
#                         runshade = 0)
#   max.5cm.soils[i] <- max(micro$soil[, 5])
# }

source('get_micro_output.R')

# locate the New et al. climate dataset
gcfolder <- paste(.libPaths()[1],"/gcfolder.rda",sep="")
if(file.exists(gcfolder) == FALSE){
  folder <- "c:/globalclimate"
  if(file.exists(paste0(folder,"/global_climate.nc")) == FALSE){
    message("You don't appear to have the global climate data set - \n run function get.global.climate(folder = 'folder you want to put it in') .....\n exiting function micro_global")
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    stop()
  }
}else{
  load(gcfolder)
}

# query New et al. climatology
global_climate <- terra::rast(paste0(folder, "/global_climate.nc"))
CLIMATE <- t(as.numeric(terra::extract(global_climate, t(matrix(loc)))))
elev_ref <- as.numeric(CLIMATE[1]) # reference elevation

source('micro_pars.R')

outputs <- matrix(nrow = nrow(grid_lonlat), ncol = 6)
outputs[, 1] <- grid_lonlat[, 1]
outputs[, 2] <- grid_lonlat[, 2]

for(i in 1:nrow(grid_lonlat)){
  
  # current point
  loc <- c(grid_lonlat[i, 1], grid_lonlat[i, 2]) # location
  azmuth <- aspects[i] # aspect
  slope <- slopes[i] # slope
  elev <- elevs[i] # elevation
  
  # update location parameters
  ALREF <- abs(loc[1])
  HEMIS <- 1 # 1 is northern hemisphere
  # break decimal degree lat/lon into deg and min
  ALAT <- abs(trunc(loc[2]))
  AMINUT <- (abs(loc[2])-ALAT)*60
  ALONG <- abs(trunc(loc[1]))
  ALMINT <- (abs(loc[1])-ALONG)*60
  
  # adjust temperature (and humidity) for lapse rate
  delta_elev <- elev_ref - elev # get delta for lapse rate correction
  adiab_corr_max <- delta_elev * lapse_max
  adiab_corr_min <- delta_elev * lapse_min
  TMAXX <- TMAXX_ref + adiab_corr_max
  TMINN <- TMINN_ref + adiab_corr_min
  # correct for change in RH with elevation-corrected Tair
  es <- WETAIR(db = TMAXX, rh = 100)$esat
  e <- WETAIR(db = TMAXX_ref, rh = RHMINN_ref)$e
  RHMINN <- (e / es) * 100
  RHMINN[RHMINN>100] <- 100
  RHMINN[RHMINN<0] <- 0.01
  es <- WETAIR(db = TMINN, rh = 100)$esat
  e <- WETAIR(db = TMINN_ref, rh = RHMAXX_ref)$e
  RHMAXX <- (e / es) * 100
  RHMAXX[RHMAXX>100] <- 100
  RHMAXX[RHMAXX<0] <- 0.01
  
  # final parameter list
  microinput <- c(doynum, RUF, ERR, Usrhyt, Refhyt, Numtyps, Z01, Z02, ZH1, ZH2, idayst, ida, HEMIS, ALAT, AMINUT, ALONG, ALMINT, ALREF, slope, azmuth, elev, CMH2O, microdaily, tannul, EC, VIEWF, snowtemp, snowdens, snowmelt, undercatch, rainmult, runshade, runmoist, maxpool, evenrain, snowmodel, rainmelt, writecsv, densfun, hourly, rainhourly, lamb, IUV, RW, PC, RL, SP, R1, IM, MAXCOUNT, IR, message, fail, snowcond, intercept, grasshade, solonly, ZH, D0, TIMAXS, TIMINS, spinup, dewrain, moiststep, maxsurf)
  # Final input list - all these variables are expected by the input argument of the Fortran microclimate subroutine
  micro <- list(microinput = microinput, tides = tides, doy = doy, SLES = SLES, DEP = DEP, Nodes = Nodes, MAXSHADES = MAXSHADES, MINSHADES = MINSHADES, TMAXX = TMAXX, TMINN = TMINN, RHMAXX = RHMAXX, RHMINN = RHMINN, CCMAXX = CCMAXX, CCMINN = CCMINN, WNMAXX = WNMAXX, WNMINN = WNMINN, TAIRhr = TAIRhr, RHhr = RHhr, WNhr = WNhr, CLDhr = CLDhr, SOLRhr = SOLRhr, RAINhr = RAINhr, ZENhr = ZENhr, IRDhr = IRDhr, REFLS = REFLS, PCTWET = PCTWETS, soilinit = soilinit, hori = hori, TAI = TAI, soilprops = soilprops, moists = moists, RAINFALL = RAINFALL, tannulrun = tannulrun, PE = PE, KS = KS, BB = BB, BD = BD, DD = DD, L = L, LAI = LAI)
  
  ### Executing the microclimate model
  microut <- microclimate(micro) # run the model in Fortran
  # retrieve output
  micro <- get_micro_output(microut)
  # run ectotherm model with default seetings
  ecto <- ectotherm(live = 0)
  # retrieve the results
  max.1cm.air <- max(micro$metout[, 3])
  max.0cm.soil <- max(micro$soil[, 3])
  max.5cm.soil <- max(micro$soil[, 5])
  max.Tc <- max(ecto$environ[, 5])
  outputs[i, 3:6] <- c(max.1cm.air, max.0cm.soil, max.5cm.soil, max.Tc)
  cat(i, '\n')
}

# put in utms as coordinates
outputs[, 1] <- grid_utm[, 1]
outputs[, 2] <- grid_utm[, 2]
outputs.df <- as.data.frame(outputs)
colnames(outputs.df) <- c('x', 'y', 'T_air', 'T_soil0cm', 'T_soil5cm', 'T_b')

points <- vect(outputs.df, geom = c("x", "y"), crs(dem)) # Replace with your UTM EPSG code
r <- rast(points_utm, resolution = res(dem)) # Adjust resolution as needed

# Rasterize the points

T_air <- rasterize(points, r, field = "T_air")
T_soil0cm <- rasterize(points, r, field = "T_soil0cm")
T_soil5cm <- rasterize(points, r, field = "T_soil5cm")
T_b <- rasterize(points, r, field = "T_b")

range <- c(10, 30)

par(mfrow = c(2, 2))
plot(slope.grid, main = 'slope')
plot(aspect.grid, main = 'aspect')
plot(T_soil0cm, range = range, main = 'T_soil0cm')
plot(T_soil5cm, range = range, main = 'T_soil5cm')

par(mfrow = c(2, 2))
plot(T_air, range = range, main = 'T_air')
plot(T_soil0cm, range = range, main = 'T_soil0cm')
plot(T_soil5cm, range = range, main = 'T_soil5cm')
plot(T_b, range = range, main = 'T_b')


