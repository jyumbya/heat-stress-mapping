# Heat Stress current, future, uncertainty, change plots by RCP, Periods

# clear your work space
rm(list = ls(all = TRUE))

# load libraries
.packages = c("rasterVis","maptools", "rgdal")
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
analysisLs <- c("current", "future", "uncertainties", "changes")
id <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# import country and water mask
country_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")
water_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/other/ug_waterbodies.shp")

# output folder
oDirPlt <- paste0(oDir, "/plots", sep="")
if (!file.exists(oDirPlt)) {dir.create(oDirPlt, recursive=T)}

livestock <- livestockLs[1]

# current heat stress plot
if (!file.exists(paste0(oDirPlt, "/", analysisLs[1], "_", var, "_", livestock, ".tif", sep=""))){
  
  cat("Plotting", analysisLs[1], "\n")
  
  hsC <- stack(paste0(iDir, "/30s/", livestock, "/runs/", var, "_", 1:12, ".tif"))
  hsC <- mask(crop(hsC, extent(country_mask)), country_mask)
  
  breaks <- 5
  zvalues <- seq(min(minValue(hsC)), max(maxValue(hsC)), length.out = breaks) # define limits
  colors <- c("red", "orange", "yellow", "grey")
  myColorkey <- list(at=zvalues,height=0.95, width=1.5, labels=list(at=zvalues+0.5,labels=c("Alert","Danger","Emergency","Normal"), cex=1.25))
  
  plot <- setZ(hsC, id)
  names(plot) <- id
  
  myTheme <- BuRdTheme() 
  myTheme$regions$col = colorRampPalette(colors)(length(zvalues)-1)
  myTheme$strip.border$col = "white" 
  myTheme$axis.line$col = 'white' 
  
  tiff(paste0(oDirPlt, "/", analysisLs[1], "_", var, "_", livestock, ".tif", sep=""), width=1000, height=1200, pointsize=8, compression="lzw", res=100)
  
  print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), 
                  layout=c(3, 4), 
                  xlab="", 
                  ylab="", 
                  par.settings = myTheme, 
                  colorkey = myColorkey,
                  margin=FALSE) 
        + latticeExtra::layer(sp.polygons(water_mask, col="blue", lwd=0.125))
        + latticeExtra::layer(sp.polygons(country_mask, col="black", lwd=0.25)))
  
  dev.off()
  
}

# future heat stress plot
for (rcp in rcpLs){
  
  livestock <- livestockLs[1]
  
  for (period in periodLs){
    
    cat("Plotting", analysisLs[2], ":", rcp, period, "\n")
    
    if (!file.exists(paste(oDirPlt, "/", analysisLs[2], "_", var, "_", livestock, "_", rcp, "_", period, ".tif", sep="")))
      
      # load future suitability in stack (all years by rcp)
      hsF <- stack(paste0(iDir, "/uncertainties/", rcp, "/", period, "/", "mode_", var, "_", 1:12, ".tif"))
      hsF <- mask(crop(hsF, extent(country_mask)), country_mask)
    
      breaks <- 5
      zvalues <- seq(min(minValue(hsF)), max(maxValue(hsF)), length.out = breaks) # Define limits
      colors <- c("red", "orange", "yellow", "gray")
      myColorkey <- list(at=zvalues,height=0.95, width=1.5, labels=list(at=zvalues+0.5,labels=c("Alert","Danger","Emergency","Normal"), cex=1.25))
    
      plot <- setZ(hsF, id)
      names(plot) <- id
    
      myTheme <- BuRdTheme() 
      myTheme$regions$col = colorRampPalette(colors)(length(zvalues)-1)
      myTheme$strip.border$col = "white" 
      myTheme$axis.line$col = 'white' 
    
      tiff(paste0(oDirPlt, "/", analysisLs[2], "_", var, "_", livestock, "_", rcp, "_", period, ".tif", sep=""), width=1000, height=1200, pointsize=8, compression="lzw", res=100)
    
      print(levelplot(plot, 
                      at = zvalues, 
                      scales = list(draw=FALSE), 
                      layout=c(3, 4), 
                      xlab="", 
                      ylab="", 
                      par.settings = myTheme,
                      colorkey = myColorkey,
                      margin=FALSE)
            + layer(sp.polygons(water_mask, col="blue", lwd=0.125))
            + layer(sp.polygons(country_mask, col="black", lwd=0.25)))
      
      dev.off()
      
  }
  
}

# uncertainty plots
for (rcp in rcpLs){
  
  livestock <- livestockLs[1]
  
  for (period in periodLs){
    
    cat("Plotting", analysisLs[3], ":", rcp, period, "\n")
    
    if (!file.exists(paste0(oDirPlt, "/", analysisLs[3], "_", var, "_", livestock, "_", rcp, "_", period, ".tif", sep="")))
      
      # load uncertainties in stack (all years by rcp)
      hsUn <- stack(paste0(iDir, "/uncertainties/", rcp, "/", period, "/", "entropy_", var, "_", 1:12, ".tif"))
      hsUn <- 1-hsUn
      hsUn <- mask(crop(hsUn*100, country_mask), country_mask)
    
      breaks <- 12
      zvalues <- seq(min(minValue(hsUn)), max(maxValue(hsUn)), length.out = breaks)
      myColorkey <- list(at=zvalues,height=0.95, width=1.5, labels=list(at=zvalues+0.5,labels=c("0", "10","20","30","40","50","60","70","80","90","100"), cex=1.25))
      
      plot <- setZ(hsUn, id)
      names(plot) <- id
    
      myTheme <- BuRdTheme() 
      myTheme$regions$col = colorRampPalette(c("red", "green"))(length(zvalues)-1)
      myTheme$strip.border$col = "white" 
      myTheme$axis.line$col = 'white'
    
      tiff(paste0(oDirPlt, "/", analysisLs[3], "_", var, "_", livestock, "_", rcp, "_", period, ".tif", sep=""), width=1000, height=1200, pointsize=8, compression="lzw", res=100)
    
      print(levelplot(plot, 
                      at = zvalues, 
                      scales = list(draw=FALSE), 
                      layout=c(3, 4), 
                      xlab="", 
                      ylab="", 
                      par.settings = myTheme,
                      colorkey = myColorkey,
                      margin=FALSE)
            + layer(sp.polygons(water_mask, col="blue", lwd=0.125))
            + layer(sp.polygons(country_mask, col="black", lwd=0.25)))
    
    dev.off()
    
  }

}

# change plots
for (rcp in rcpLs){
  
  livestock <- livestockLs[1]
  
  for (period in periodLs){
    
    cat("Plotting", analysisLs[4], ":", rcp, period, "\n")
    
    if (!file.exists(paste0(oDirPlt, "/", analysisLs[4], "_", var, "_", livestock, "_", rcp, "_", period, ".tif", sep="")))
      
      # load changes in stack (all years by rcp)
      hCng <- stack(paste0(iDir, "/changes/", rcp, "/", period, "/", var, "_cng", "_", 1:12, ".tif"))
      hCng <- mask(crop(hCng, country_mask), country_mask)
    
      breaks <- 17
      zvalues <- seq(min(minValue(hCng)), max(maxValue(hCng)), length.out = breaks) # Define limits
      colors <- c("red", "yellowgreen", "deepskyblue4", "springgreen4", "orangered3", "yellow4", "deepskyblue", "springgreen", "indianred3", "yellow", "cyan4", "wheat4", "lightcoral", "khaki", "cyan", "wheat")
    
      myColorkey <- list(at=zvalues,height=0.95, width=1.5, labels=list(at=zvalues+0.5,labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), cex=1.1))
    
      plot <- setZ(hCng, id)
      names(plot) <- id
    
      myTheme <- BuRdTheme() 
      myTheme$regions$col = colorRampPalette(colors)(length(zvalues)-1)
      myTheme$strip.border$col = "white" 
      myTheme$axis.line$col = 'white' 
    
      tiff(paste0(oDirPlt, "/", analysisLs[4], "_", var, "_", livestock, "_", rcp, "_", period, ".tif", sep=""), width=1000, height=1200, pointsize=8, compression="lzw", res=100)
    
      print(levelplot(plot, 
                      at = zvalues, 
                      scales = list(draw=FALSE), 
                      layout=c(3, 4), 
                      xlab="", 
                      ylab="", 
                      par.settings = myTheme, 
                      colorkey = myColorkey,
                      margin=FALSE)
            + latticeExtra::layer(sp.polygons(water_mask, col="blue", lwd=0.125))
            + latticeExtra::layer(sp.polygons(country_mask, col="black", lwd=0.25)))
      dev.off()
      
  }
  
}

#plot pig index change matrix
tiff(paste0(oDirPlt , "/change_matrix.tif"), width=600, height=600, pointsize=12, compression='lzw',res=100)

pig.index.change.matrix()

dev.off()
