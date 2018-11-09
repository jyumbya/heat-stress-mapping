# Calculate current heat stress
# Based on Heat Stress Index for grow-finish swine, chart by IOWA State University
# Author: J.Mutua, CIAT
# Last modified: 18/09/2017

#clear your work space
rm(list = ls(all = TRUE))

# load packages
.packages = c("raster", "rgdal")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat-stress-mapping/scripts/00_hs_functions.R")

# set variables
cDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/climate_data/current/worldclim_v1.4_1KM"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping"

# create output folder
if(!dir.exists(paste0(oDir, '/outputs/raster_current', sep=''))) {dir.create(paste0(oDir, '/outputs/raster_current', sep=''))}
   
# read maximum temperature files
tmax.files <- list.files(paste0(cDir), pattern = '^t.*.tif$',full.names = T)
tmax.raster <- lapply(tmax.files,raster)

# read relative humidity files
rh.files <- list.files(paste0(cDir), pattern = '^r.*.tif$',full.names = T)
rh.raster <- lapply(rh.files,raster)

# calculate monthly heat stress
lapply(1:12,function(w){
  x = tmax.raster[[w]]/10 # temp is multiplied by 10
  y = rh.raster[[w]]
  
  h <- y
  h[] <- pig.index(x,y)
  
writeRaster(h, filename = paste0(oDir, '/outputs/raster_current/current_hs_',w,".tiff"), format = "GTiff", datatype='INT4S', overwrite=TRUE)
  return(print('OK'))
}  
)
