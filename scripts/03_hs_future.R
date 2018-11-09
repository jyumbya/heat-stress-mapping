# Calculate future heat stress
# Author: John Mutua, CIAT
# Last modified: 18/09/2017

# clear your work space
rm(list = ls(all = TRUE))

# load packages
.packages = c("rgdal","raster", "gtools")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
cDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/climate_data/future"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat_stress_mapping/_scripts/00_hs_functions.R")

# loop rcps, gcms and periods
for (rcp in rcpLs){
  
  gcmList <- list.dirs(paste0(cDir, "/", rcp, "/global_30s"), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList){
    
    for (period in periodLs){
      
      if(!file.exists(paste0(oDir, "/outputs/raster_future","/runs-", rcp, "/", gcm , "/", period, sep=""))){dir.create(paste0(oDir, "/outputs/raster_future","/runs-", rcp, "/", gcm , "/", period, sep=""), recursive=T)}
        
        # start heat stress analysis
        cat("Processing : ",  rcp, gcm, "\n")
        
        # read maximum temperature files
        tmax.files <- list.files(paste0(cDir, "/", rcp, "/global_30s/", gcm, "/r1i1p1/", period, "/_tif"), pattern = '^tmax.*.tif$', full.names = T)
        tmax.files <- gtools::mixedsort(tmax.files) # reorganise files
        tmax.raster <- lapply(tmax.files,raster)
        
        # read relative humidity files
        rh.files <- list.files(paste0(cDir, "/", "rh/"), pattern = '*.tif$', full.names = T)
        rh.raster <- lapply(rh.files,raster)
        
        # calculate monthly heat stress
        lapply(1:12,function(w){
          x = tmax.raster[[w]]/10 # since temperature is multiplied by 10
          y = rh.raster[[w]]
          
          h <- y
          h[] <- pig.index(x,y)
          writeRaster(h, filename = paste0(oDir, "/outputs/raster_future/", "runs-", rcp, "/", gcm, "/", period, "/hs_", w,".tiff"), format = "GTiff", overwrite=TRUE)
        })
        
        cat(paste("Processed : ", rcp, gcm, "\n"))
    }
  }
}


