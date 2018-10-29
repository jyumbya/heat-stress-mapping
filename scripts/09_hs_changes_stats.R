# Heat Stress changes - statistics
# Author: John Mutua, CIAT
# Last modified: 11/12/2017

# clear your work space
rm(list = ls(all = TRUE))

# load libraries
.packages = c("raster", "maptools", "rgdal", "sp")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/raster_changes"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
mthLS <- c(1:12)
ug_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")
ug_mask_districts <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/Pig_VC_districts.shp")

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat_stress_mapping/_scripts/00_hs_functions.R")

# calculate change statistics for the study area
rcpList <- lapply(X = rcpLs, FUN = function(rcp){
  
  periodList <- lapply(X = periodLs, FUN = function(period){
    
    mthList <- lapply(X = mthLS, FUN = function(mth){
      
      #load change raster
      cng <- raster(paste0(iDir, "/", rcp, "/", period, "/cng_", mth, ".tif", sep=""))
      
      rs <- mask(crop(cng, ug_mask), ug_mask)
      
      #calculate change areas
      x <- rs
      b <- hs.change.stats(x)
      c <- as.data.frame(b) #convert to dataframe
      d <- cbind(RCP=rep(rcp,times=nrow(c)), PERIOD=rep(period,times=nrow(c)), MONTH=rep(mth,times=nrow(c)), c) #add in rcp, period, month
      return(d) #add into the list
      
    })
    mthList <- do.call(rbind, mthList)
    return(mthList)
    
  })
  periodList <- do.call(rbind, periodList)
  return(periodList)
  
})
rcpList <- do.call(rbind, rcpList)

write.csv(rcpList, paste0(oDir, "/changes_stats/","hs_change_stats.csv"), row.names = F)