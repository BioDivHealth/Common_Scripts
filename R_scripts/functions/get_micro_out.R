get_micro_output <- function(microut){
  metout <- microut$metout # retrieve above ground microclimatic conditions, min shade
  shadmet <- microut$shadmet # retrieve above ground microclimatic conditions, max shade
  soil <- microut$soil # retrieve soil temperatures, minimum shade
  shadsoil <- microut$shadsoil # retrieve soil temperatures, maximum shade
  tcond <- microut$tcond
  shadtcond <- microut$shadtcond
  specheat <- microut$specheat
  shadspecheat <- microut$shadspecheat
  densit <- microut$densit
  shaddensit <- microut$shaddensit
  if(runmoist == 1){
    soilmoist <- microut$soilmoist # retrieve soil moisture, minimum shade
    shadmoist <- microut$shadmoist # retrieve soil moisture, maximum shade
    humid <- microut$humid # retrieve soil humidity, minimum shade
    shadhumid <- microut$shadhumid # retrieve soil humidity, maximum shade
    soilpot <- microut$soilpot # retrieve soil water potential, minimum shade
    shadpot <- microut$shadpot # retrieve soil water potential, maximum shade
    plant <- microut$plant # retrieve plant output, minimum shade
    shadplant <- microut$shadplant # retrieve plant output, maximum shade
  }else{
    soilpot <- soil
    soilmoist <- soil
    shadpot <- soil
    shadmoist <- soil
    humid <- soil
    shadhumid <- soil
    plant <- cbind(soil,soil[,3:4])
    shadplant <- cbind(soil,soil[,3:4])
    soilpot[,3:12] <- 0
    soilmoist[,3:12] <- 0.5
    shadpot[,3:12] <- 0
    shadmoist[,3:12] <- 0.5
    humid[,3:12] <- 0.99
    shadhumid[,3:12] <- 0.99
    plant[,3:14] <- 0
    shadplant[,3:14] <- 0
  }
  if(snowmodel == 1){
    sunsnow <- microut$sunsnow
    shdsnow <- microut$shdsnow
  }
  
  if(max(metout[,1] == 0)){
    cat("ERROR: the model crashed - try a different error tolerance (ERR) or a different spacing in DEP", '\n')
  }
  days <- rep(seq(1,timeinterval * nyears), 24)
  days <- days[order(days)]
  dates <- days + metout[, 2] / 60 / 24 - 1 # dates for hourly output
  dates2 <- seq(1, timeinterval * nyears) # dates for daily output
  if(lamb == 1){
    drlam <- as.data.frame(microut$drlam) # retrieve direct solar irradiance
    drrlam <- as.data.frame(microut$drrlam) # retrieve direct Rayleigh component solar irradiance
    srlam <- as.data.frame(microut$srlam) # retrieve scattered solar irradiance
    if(snowmodel == 1){
      return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,soilmoist=soilmoist,shadmoist=shadmoist,humid=humid,shadhumid=shadhumid,soilpot=soilpot,shadpot=shadpot,sunsnow=sunsnow,shdsnow=shdsnow,plant=plant,shadplant=shadplant,tcond=tcond,shadtcond=shadtcond,specheat=specheat,shadspecheat=shadspecheat,densit=densit,shaddensit=shaddensit,RAINFALL=RAINFALL,ndays=ndays,elev=ALTT,REFL=REFL[1],longlat=c(loc[1],loc[2]),nyears=nyears,timeinterval=timeinterval,minshade=MINSHADES,maxshade=MAXSHADES,DEP=DEP,drlam=drlam,drrlam=drrlam,srlam=srlam,dates=dates,dates2=dates2,PE=PE,BD=BD,DD=DD,BB=BB,KS=KS,dem=dem, diffuse_frac = diffuse_frac))
    }else{
      return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,soilmoist=soilmoist,shadmoist=shadmoist,humid=humid,shadhumid=shadhumid,soilpot=soilpot,shadpot=shadpot,plant=plant,shadplant=shadplant,tcond=tcond,shadtcond=shadtcond,specheat=specheat,shadspecheat=shadspecheat,densit=densit,shaddensit=shaddensit,RAINFALL=RAINFALL,ndays=ndays,elev=ALTT,REFL=REFL[1],longlat=c(loc[1],loc[2]),nyears=nyears,timeinterval=timeinterval,minshade=MINSHADES,maxshade=MAXSHADES,DEP=DEP,drlam=drlam,drrlam=drrlam,srlam=srlam,dates=dates,dates2=dates2,PE=PE,BD=BD,DD=DD,BB=BB,KS=KS,dem=dem, diffuse_frac = diffuse_frac))
    }
  }else{
    if(snowmodel == 1){
      return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,soilmoist=soilmoist,shadmoist=shadmoist,humid=humid,shadhumid=shadhumid,soilpot=soilpot,shadpot=shadpot,sunsnow=sunsnow,shdsnow=shdsnow,plant=plant,shadplant=shadplant,tcond=tcond,shadtcond=shadtcond,specheat=specheat,shadspecheat=shadspecheat,densit=densit,shaddensit=shaddensit,RAINFALL=RAINFALL,ndays=ndays,elev=ALTT,REFL=REFL[1],longlat=c(loc[1],loc[2]),nyears=nyears,timeinterval=timeinterval,minshade=MINSHADES,maxshade=MAXSHADES,DEP=DEP,dates=dates,dates2=dates2,PE=PE,BD=BD,DD=DD,BB=BB,KS=KS,dem=dem, diffuse_frac = diffuse_frac))
    }else{
      return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,soilmoist=soilmoist,shadmoist=shadmoist,humid=humid,shadhumid=shadhumid,soilpot=soilpot,shadpot=shadpot,plant=plant,shadplant=shadplant,tcond=tcond,shadtcond=shadtcond,specheat=specheat,shadspecheat=shadspecheat,densit=densit,shaddensit=shaddensit,RAINFALL=RAINFALL,ndays=ndays,elev=ALTT,REFL=REFL[1],longlat=c(loc[1],loc[2]),nyears=nyears,timeinterval=timeinterval,minshade=MINSHADES,maxshade=MAXSHADES,DEP=DEP,dates=dates,dates2=dates2,PE=PE,BD=BD,DD=DD,BB=BB,KS=KS,dem=dem, diffuse_frac = diffuse_frac))
    }
  }
}