# Heat Stress changes - statistics
# Author: John Mutua, CIAT
# Last modified: 11/12/2017

# clear your work space
rm(list = ls(all = TRUE))

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat-stress-mapping/scripts/00_hs_functions.R")

# set variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/raster_changes"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
mthLs <- c(1:12)
shp <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")
shp.poly <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/Pig_VC_districts.shp")

# calculate change statistics for the study area
rcpStats <- lapply(X = rcpLs, FUN = function(rcp){
  
  periodStats <- lapply(X = periodLs, FUN = function(period){
    
    mthStats <- lapply(X = mthLs, FUN = function(mth){
      
      #load change raster
      cng <- raster(paste0(iDir, "/", rcp, "/", period, "/cng_", mth, ".tif", sep=""))
      
      rs <- mask(crop(cng, shp), shp)
      
      #calculate change areas
      x <- rs
      b <- pig.index.change.stats(x)
      c <- as.data.frame(b) #convert to dataframe
      d <- cbind(RCP=rep(rcp,times=nrow(c)), PERIOD=rep(period,times=nrow(c)), MONTH=rep(mth,times=nrow(c)), c) #add in rcp, period, month
      return(d) #add into the list
      
    })
    mthStats <- do.call(rbind, mthStats)
    return(mthStats)
    
  })
  periodStats <- do.call(rbind, periodStats)
  return(periodStats)
  
})
rcpStats <- do.call(rbind, rcpStats)

write.csv(rcpStats, paste0(oDir, "/changes_stats/","hs_change_stats.csv"), row.names = F)
