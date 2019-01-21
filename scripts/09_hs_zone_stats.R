# Heat Stress zone - statistics
# Author: John Mutua, CIAT
# Last modified: 21/01/2019

# clear your work space
rm(list = ls(all = TRUE))

# load libraries
.packages = c("raster","rgdal")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)


# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat-stress-mapping/scripts/00_hs_functions.R")

# set variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
var <- "hs"
livestockLs <- c("pig", "cattle")
mthLs <- c(1:12)
country_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")
#districts_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/Pig_VC_districts.shp")

# output folder
oDirStats <- paste0(oDir, "/stats", sep="")
if (!file.exists(oDirStats)) {dir.create(oDirStats, recursive=T)}

# current analysis
mthStats <- lapply(X = mthLs, FUN = function(mth){
  
  nPol <- length(country_mask@polygons)
  
  shpStats <- lapply(1:nPol, function(p){
    
    shpStats <- list()
    cname <- country_mask@data$COUNTRY[p]
    pol <- country_mask@polygons[p]
    sh <- SpatialPolygons(pol) 
    
    #load current raster
    hsC <- raster(paste0(iDir, "/30s/", livestockLs[1], "/runs/", var, "_", mth, ".tif", sep=""))
    
    rs <- mask(crop(hsC, country_mask), country_mask)
    
    #calculate change areas
    x <- rs
    b <- pig.index.zone.stats(x)
    c <- as.data.frame(b) #convert to dataframe
    d <- cbind(DISTRICT=rep(cname,times=nrow(c)), MONTH=rep(mth,times=nrow(c)), c) #add in rcp, period
    
    return(d)
    
  })
  shpStats <- do.call("rbind", shpStats)
  
  return(shpStats)
  
})
mthStats <- do.call(rbind, mthStats)

write.csv(mthStats, paste0(oDirStats, "/","pig_hs_zone_area_current_stats.csv"), row.names = F)




# future analysis
rcpStats <- lapply(X = rcpLs, FUN = function(rcp){
  
  periodStats <- lapply(X = periodLs, FUN = function(period){
    
    mthStats <- lapply(X = mthLs, FUN = function(mth){
      
      nPol <- length(country_mask@polygons)
      
      shpStats <- lapply(1:nPol, function(p){
        
        shpStats <- list()
        cname <- country_mask@data$COUNTRY[p]
        pol <- country_mask@polygons[p]
        sh <- SpatialPolygons(pol) 
        
        #load future raster
        hsF <- raster(paste0(iDir, "/uncertainties/", rcp, "/", period, "/mode_", var, "_", mth, ".tif", sep=""))
        
        rs <- mask(crop(hsF, country_mask), country_mask)
        
        #calculate change areas
        x <- rs
        b <- pig.index.zone.stats(x)
        c <- as.data.frame(b) #convert to dataframe
        d <- cbind(DISTRICT=rep(cname,times=nrow(c)), RCP=rep(rcp,times=nrow(c)), PERIOD=rep(period,times=nrow(c)), MONTH=rep(mth,times=nrow(c)), c) #add in rcp, period
        
        return(d)
        
      })
      shpStats <- do.call("rbind", shpStats)
      
      return(shpStats)
      
    })
    mthStats <- do.call(rbind, mthStats)
    return(mthStats)
    
  })
  periodStats <- do.call(rbind, periodStats)
  return(periodStats)
  
})
rcpStats <- do.call(rbind, rcpStats)

write.csv(rcpStats, paste0(oDirStats, "/","pig_hs_zone_area_future_stats.csv"), row.names = F)
