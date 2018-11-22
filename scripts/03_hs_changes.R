# clear your work space
rm(list = ls(all = TRUE))

# load libraries
.packages = c("raster","rgdal", "maptools", "rasterVis")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set some variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
var <- "hs"
analysisLs <- c("current", "future")
livestockLs <- c("pig", "cattle")

country_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat-stress-mapping_ANNUAL/scripts/00_hs_functions.R")

for (rcp in rcpLs){
  
  livestock <- livestockLs[1]
  
  for (period in periodLs){
    
    oDirChn <- paste0(oDir, "/changes", "/", rcp, "/", period, sep="")
    if (!file.exists(oDirChn)) {dir.create(oDirChn, recursive=T)}
    
    cat("Processing change: ",  rcp, period, "\n")
    
    for (mth in 1:12){
      
      hsC <- raster(paste0(iDir, "/30s/", livestock, "/runs/", var, "_", mth, ".tif"))
      
      hsF <- raster(paste0(iDir, "/uncertainties/", rcp, "/", period, "/", "mode_", var, "_", mth, ".tif"))
      hsF <- resample(crop(hsF, hsC), hsC)
      
      x <- hsC
      y <- hsF
    
      hs.change <- overlay(x,y, fun=pig.index.change)
    
      writeRaster(hs.change, filename = paste0(oDirChn, "/hs_cng", "_", mth), format = "GTiff", datatype='INT4S', overwrite=TRUE)
    
    }

  }

}

