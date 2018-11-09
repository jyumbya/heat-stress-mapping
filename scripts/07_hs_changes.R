# Heat stress changes
# Author: John Mutua, CIAT
# Last modified: 11/11/2017

# clear your work space
rm(list = ls(all = TRUE))

# load libraries
.packages = c("raster","rgdal", "maptools", "rasterVis")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat-stress-mapping/scripts/00_hs_functions.R")

# set some variables
cDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/raster_current"
fDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/uncertainties_future"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/raster_changes"
var = "hs"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")


# plot by month
for (rcp in rcpLs){
  
  for (period in periodLs){
    
    if(!file.exists(paste0(oDir, "/", rcp, "/", period, sep=""))){dir.create(paste0(oDir, "/", rcp, "/", period, sep=""), recursive=T)}
    
    cat("Processing : ",  rcp, period, "\n")
    
    current <- list.files(paste0(cDir, "/"), pattern = '*.tif$',full.names = T)
    current <- gtools::mixedsort(current) # reorganise files
    current <- lapply(current,raster)
      
    future <- list.files(paste0(fDir, "/", rcp, "/", period), pattern = '^m.*.tif$', full.names = T)
    future <- gtools::mixedsort(future)
    future <- lapply(future,raster)
    
    lapply(1:12,function(w){
      x = current[[w]]
      y = future[[w]]
      
      h <- y
      h[] <- pig.index.change(x,y)
      
      writeRaster(h, filename = paste0(oDir, "/", rcp, "/", period, "/cng_", w,".tiff"), format = "GTiff", overwrite=TRUE)
      }
      )
    cat(paste("Processed : ", rcp, period, "\n"))
  }
}

