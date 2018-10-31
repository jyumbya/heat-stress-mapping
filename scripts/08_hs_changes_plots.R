# Heat Stress changes plots
# Author: John Mutua, CIAT
# Last modified: 11/12/2017

# clear your work space
rm(list = ls(all = TRUE))

# load libraries
.packages = c("rgdal", "maptools", "rasterVis")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/raster_changes"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
var <- "hs"
id <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# import country mask
ug_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")

# import water mask
water_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/other/ug_waterbodies.shp")

for (rcp in rcpLs){
  
  for (period in periodLs){
    
    pDir <- paste0("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/plots_changes/")
    if (!file.exists(pDir)) {dir.create(pDir, recursive=T)}
    
    cat("Processing : ",  rcp, period, "\n")
    
    var_stk <- stack(paste0(iDir, "/", rcp, "/", period, "/", "cng", "_", 1:12, ".tif"))
    var_stk_msk <- mask(crop(var_stk, ug_mask), ug_mask)
    
    breaks <- 17
    zvalues <- seq(min(minValue(var_stk_msk)), max(maxValue(var_stk_msk)), length.out = breaks) # Define limits
    colors <- c("red", "yellowgreen", "deepskyblue4", "springgreen4", "orangered3", "yellow4", "deepskyblue", "springgreen", "indianred3", "yellow", "cyan4", "wheat4", "lightcoral", "khaki", "cyan", "wheat")
    myColorkey <- list(at=zvalues,height=0.95, width=1.5, labels=list(at=zvalues+0.5,labels=c("HS alert to HS alert",
                                                                                              "HS alert to HS danger",
                                                                                              "HS alert to HS emergency",
                                                                                              "HS alert to No HS",
                                                                                              "HS danger to HS alert",
                                                                                              "HS danger to HS danger",
                                                                                              "HS danger to HS emergency",
                                                                                              "HS danger to No HS",
                                                                                              "HS emergency to HS alert",
                                                                                              "HS emergency to HS danger",
                                                                                              "HS emergency to HS emergency",
                                                                                              "HS emergency to No HS",
                                                                                              "No HS to HS alert",
                                                                                              "No HS to HS danger",
                                                                                              "No HS to HS emergency",
                                                                                              "No HS to No HS"),
                                                                      cex=1.1))
    #myColorkey <- list(at=zvalues,height=0.95, width=1.5, labels=list(at=zvalues+0.5,labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), cex=1.1))

    plot <- setZ(var_stk_msk, id)
    names(plot) <- id
    
    myTheme <- BuRdTheme() 
    myTheme$regions$col = colorRampPalette(colors)(length(zvalues)-1)
    myTheme$strip.border$col = "white" 
    myTheme$axis.line$col = 'white' 
    
    tiff(paste0(pDir, "/", rcp, "_", period, "_", "change", ".tif"), width=1000, height=1200, pointsize=8, compression="lzw", res=100)
    
    print(levelplot(plot, 
                    at = zvalues, 
                    scales = list(draw=FALSE), 
                    layout=c(3, 4), 
                    xlab="", 
                    ylab="", 
                    par.settings = myTheme, 
                    colorkey = myColorkey,
                    margin=FALSE
                    )
          + layer(sp.polygons(water_mask, col="blue", lwd=0.125))
          + layer(sp.polygons(ug_mask, col="black", lwd=0.25))
          )
    dev.off()
    cat(paste("Processed : ", rcp, period, "\n"))
  }
}

