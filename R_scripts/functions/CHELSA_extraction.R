# Install packages ----
if(!require(devtools)) install.packages("devtools")
library(devtools)
devtools::install_github("HelgeJentsch/ClimDatDownloadR", force = TRUE)
library(ClimDatDownloadR)


Chelsa.Clim.download(
  save.location = "/Volumes/T7 Shield", 
  # the parameters must be a string-vector input. 
  # Single parameters, however, can be just put in as a string. 
  # the valid parameter inputs can be found in the help (linked s.o.)
  parameter = c("bio"),
  # Now, since you chose "temp" and "bio" as input parameters, 
  # you can specify the months and bioclim-variables to download. 
  # If you want all of them, just leave the default values.
  # It is crutial, however, that the inputs are integer number values.
  month.var = c(1), # Here January was chosen to be downloaded for demonstration purposes
  bio.var =  c(1), # Here the first bioclim-variable was chosen to be downloaded for demonstration purposes
  # For Chelsa a newer Version of their climatologies was published in 2019.
  # They still got their old version still hosted on their website. 
  # So you can download it as well, if you want to reproduce some research you base your studies on. 
  version.var = "2.1", # Here the newer version is chosen
  # Now you can choose whether you want the data set clipped
  clipping = TRUE, # Here TRUE was chosen to show a basic introduction to the function
  # Since "clipping" is enganged now you can specify the extent you want to have for your analysis
  # This is possible via the parameters "clip.shapefile", "clip.extent", and "buffer"
  clip.extent = c(-9,20,35,80), # Here the extent for Europe was used ... 
  buffer = 5, # ... with a 5 arc-degree buffer.
  # Now, since some might prefer older file formats there is a possibility to convert 
  # clipped files and raw data into ESRI-ASCII format
  convert.files.to.asc = FALSE, 
  # now you can stack the data ...
  stacking.data = FALSE, 
  # ... and choose if you want to combine the raw data in a .zip-file ...
  combine.raw.zip = FALSE,
  # and whether raw data should be deleted.
  delete.raw.data = FALSE,
  # Finally you are presented with the option to save a bibliography file at the save location. 
  save.bib.file = TRUE
)
