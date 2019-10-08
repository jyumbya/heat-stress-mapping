# Heat stress impacts on pigs in the future
# Author: John Mutua, CIAT
# Last modified: 11/12/2017

# clear your work space
rm(list = ls(all = TRUE))

# load libraries
.packages = c("raster", "rgdal", "ggplot2")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
mthLs <- c(1:12)
var <- "hs"
livestockLs <- c("pig", "cattle")
country_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")

# output folder
oDirPlt <- paste0(oDir, "/plots", sep="")
if (!file.exists(oDirPlt)) {dir.create(oDirPlt, recursive=T)}

# output folder
oDirStats <- paste0(oDir, "/stats", sep="")
if (!file.exists(oDirStats)) {dir.create(oDirStats, recursive=T)}

livestock<-livestockLs[1]

rcpStats <- lapply(X = rcpLs, FUN = function(rcp){
  
  periodStats <- lapply(X = periodLs, FUN = function(period){
    
    mthStats <- lapply(X = mthLs, FUN = function(mth){
      
      # load future heat stress layer
      cng <- raster(paste0(iDir, "/outputs/changes/", rcp, "/", period, "/", "hs_cng", "_", mth, ".tif"))
      cng <- mask(crop(cng, country_mask), country_mask)
      
      # load current pig density layer
      pig_dens <- raster(paste0(iDir, "/data/pig_density/Pigs_CC2006_AD", ".tif", sep=""))
      pig_dens <- mask(crop(pig_dens, country_mask), country_mask)
      
      # calculate zonal statistics
      zonal_hs <- zonal(pig_dens, cng, 'sum', na.rm=TRUE) 
      c <- zonal_hs #rename dataframe
      d <- cbind(RCP=rep(rcp,times=nrow(c)), PERIOD=rep(period,times=nrow(c)), MONTH=rep(mth,times=nrow(c)), c) #add in rcp, period
      return(d)
      
    })
    mthStats <- do.call(rbind, mthStats)
    return(mthStats)
    
  })
  periodStats <- do.call(rbind, periodStats)
  return(periodStats)
  
})
rcpStats <- do.call(rbind, rcpStats)
rcpStats<-as.data.frame(rcpStats) # convert to dataframe
rcpStats$sum <- as.numeric(as.character(rcpStats$sum)) # convert "sum" column to numeric
rcpStats$sum<-round(rcpStats$sum) # round column

write.csv(rcpStats, paste0(oDirStats, "/", "future_", var, "_changes_", livestock, "_impacts.csv"), row.names = F)