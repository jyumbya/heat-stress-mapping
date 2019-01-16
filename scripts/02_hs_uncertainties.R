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

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat-stress-mapping/scripts/00_hs_functions.R")

# set variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/30s"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
var <- "hs"
livestockLs <- c("pig", "cattle")

# loop rcps, gcms and periods
for (rcp in rcpLs){
  
  livestock <- livestockLs[1]
  
  rcpDir <- paste0(iDir, "/", livestock, "/runs-", rcp)
  gcmList <- list.dirs(paste0(rcpDir, "/"), recursive = FALSE, full.names = FALSE)

  for (period in periodLs){
    
    oDirUnc <- paste0(oDir, "/uncertainties/", rcp, "/", period, sep="")
    if (!file.exists(oDirUnc)) {dir.create(oDirUnc, recursive=T)}
    
    for (mth in 1:12){
      
      cat(paste("Processing : ", rcp, period, mth, "\n"))
      
      # calculate mode
      hsMode <- calc(stack(paste0(rcpDir, "/", gcmList, "/", period, "/", var, "_", mth, ".tif")), fun=categorical.mode)
      writeRaster(hsMode, filename = paste0(oDirUnc, "/mode_", var, "_", mth, ".tif", sep=""), overwrite=TRUE, datatype="INT2S")
      
      #% agreement per pixel
      hsMode <- paste0(oDirUnc, "/mode_", var, "_", mth, ".tif", sep="")
      all.stack<-stack(hsMode, stack(paste0(rcpDir, "/", gcmList, "/", period, "/", var, "_", mth, ".tif")))
      hsAgreement <- calc(all.stack, fun=agreement.pixel)*100/18
      writeRaster(hsAgreement, filename = paste0(oDirUnc, "/agreement_", var, "_", mth, ".tif", sep=""), overwrite=TRUE, datatype="INT2S")

      # calculate entropy  
      hsEntropy <- calc(stack(paste0(rcpDir, "/", gcmList, "/", period, "/", var, "_", mth, ".tif")), fun=categorical.entropy)
      hsEntropy[is.na(hsEntropy)] <- 0
      writeRaster(hsEntropy, filename = paste0(oDirUnc, "/entropy_", var, "_", mth, ".tif", sep=""), overwrite=TRUE, datatype="INT2S")
      
      
    }
    
  }

}