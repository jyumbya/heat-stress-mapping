# Calculate current and future heat stress
# Author: John Mutua, CIAT
# Last modified: 13/11/2018

# clear your work space
rm(list = ls(all = TRUE))

# load packages
.packages = c("rgdal","raster", "gtools")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/climate_data"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
mthLs <- c(paste0("0",1:9), 10:12)
analysisLs <- c("current", "future")
varLs <- c("tmax", "rh")
livestockLs <- c("pig", "cattle")

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat-stress-mapping/scripts/00_hs_functions.R")

cat(paste("Processing", analysis, "\n"))

livestock <- livestockLs[1]
analysis <- analysisLs[1]

# create output folder current outputs
oDirC <- paste0(oDir, "/", "/outputs/30s/", livestock, "/runs", sep="")
if (!file.exists(oDirC)) {dir.create(oDirC, recursive=T)}

# read maximum temperature files
tmax.raster <- stack(lapply(paste0(iDir, "/", analysis, "/worldclim_v1.4_1KM/", varLs[1], "_", mthLs, ".tif"), FUN=raster))

# read relative humidity files
rh.raster <- stack(lapply(paste0(iDir, "/", analysis, "/worldclim_v1.4_1KM/", varLs[2], "_", mthLs, ".tif"), FUN=raster))

# calculate monthly heat stress
lapply(1:12,function(w){
  x = tmax.raster[[w]]/10 # temp is multiplied by 10
  y = rh.raster[[w]]
  
  # calculate HS based on x and y
  index.result <- y
  index.result[] <- pig.index(x,y)
  
  # write output
  writeRaster(index.result, filename = paste0(oDirC, "/", "hs_", w, ".tif"), format = "GTiff", datatype='INT4S', overwrite=TRUE)
  
}  
)



# loop rcps, gcms and periods
for (rcp in rcpLs){
  
  livestock <- livestockLs[1]
  analysis <- analysisLs[2]
  
  cat(paste("Processing", analysis, ":", rcp, "\n"))
  
  # list gcms
  gcmLs <- list.dirs(paste0(iDir, "/", analysis, "/", rcp, "/global_30s"), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmLs){
    
    for (period in periodLs){
      
      # create output folder future outputs
      oDirF <- paste0(oDir, "/outputs/30s/", livestock, "/runs-", rcp, "/", gcm , "/", period, sep="")
      if (!file.exists(oDirF)) {dir.create(oDirF, recursive=T)}
      
      # read maximum temperature files
      tmax.raster <- stack(lapply(paste0(iDir, "/", analysis, "/", rcp, "/global_30s/", gcm, "/r1i1p1/", period, "/_tif/", varLs[1], "_", 1:12, ".tif"), FUN=raster))
      
      # read relative humidity files
      rh.raster <- stack(lapply(paste0(iDir, "/", analysis, "/", varLs[2], "/", varLs[2], "_", mthLs, ".tif"), FUN=raster))
      
      # calculate monthly heat stress
      lapply(1:12,function(w){
        x = tmax.raster[[w]]/10 # temp is multiplied by 10
        y = rh.raster[[w]]
        
        # calculate HS based on x and y
        index.result <- y
        index.result[] <- pig.index(x,y)
        
        # write output
        writeRaster(index.result, filename = paste0(oDirF, "/", "hs_", w, ".tif"), format = "GTiff", datatype='INT4S', overwrite=TRUE)
        
      }  
      )
      
    }
  }
}
  

