# Create stack, then a function to calculate per pixel the mode and the entropy
# Uncertainty metrics for a given stack (likely a set of previously loaded GCMs)
# Author: John Mutua, CIAT
# Last modified: 24/10/2017

# clear your work space
rm(list = ls(all = TRUE))

# load libraries
.packages = c("rgdal","raster", "DescTools")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
cDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/raster_future"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/uncertainties_future"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
var <- "hs"

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat_stress_mapping/_scripts/00_hs_functions.R")

#loop rcps, gcms and periods
for (rcp in rcpLs){
  
  rcpDir <- paste0(cDir, "/runs-", rcp)
  gcmList <- list.dirs(paste0(rcpDir, "/"), recursive = FALSE, full.names = FALSE)

  for (period in periodLs){
    
    oDir <- paste0("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/uncertainties_future/", rcp, "/", period, sep="")
    if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
    
        # read heat stress files
    for (mth in 1:12){
      
      cat("Processing : ",  rcp, period, mth, "\n")
      
      hsMode <- calc(stack(paste0(rcpDir, "/", gcmList, "/", period, "/", var, "_", mth, ".tif")), fun=categorical.mode)
      writeRaster(hsMode, filename = paste0(oDir, "/mode_", "hs_", mth, ".tif", sep=""), overwrite=TRUE)
	    
      hsEntropy <- calc(stack(paste0(rcpDir, "/", gcmList, "/", period, "/", var, "_", mth, ".tif")), fun=categorical.entropy)
      writeRaster(hsEntropy, filename = paste0(oDir, "/entropy_", "hs_", mth, ".tif", sep=""), overwrite=TRUE)
      
      cat(paste("Processed : ", rcp, period, mth, "\n"))
      
    }
    
  }

}
