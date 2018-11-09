# Heat Stress future uncertainties plots by RCP, Periods
# Author: John Mutua, CIAT
# Last modified: 22/10/2017

# clear your work space
rm(list = ls(all = TRUE))

# load libraries
.packages = c("rasterVis","maptools", "rgdal")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/uncertainties_future"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069")
var <- "entropy_hs"
id <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# import country mask
ug_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")

# import water mask
water_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/other/ug_waterbodies.shp")

for (rcp in rcpLs){
  
  for (period in periodLs){
    
    oDir <- paste0("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/plots_uncertainties/")
    if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
    
    cat("Processing : ",  rcp, period, "\n")
    
    var_stk <- stack(paste0(iDir, "/", rcp, "/", period, "/", var, "_", 1:12, ".tif"))
    var_stk[is.na(var_stk)] <- 0
    var_stk <- 1-var_stk
    var_stk_msk <- mask(crop(var_stk*100, ug_mask), ug_mask)
    
    # breaks <- 10
    zvalues <- seq(0, 100, 10)
    myColorkey <- list(at=zvalues,height=0.95, width=1.5, labels=list(at=zvalues+0.5,labels=c("10","20","30","40","50","60","70","80","90","100"), cex=1.25))

    plot <- setZ(var_stk_msk, id)
    names(plot) <- id
    
    myTheme <- BuRdTheme() 
    myTheme$regions$col = colorRampPalette(c("red", "green"))
    myTheme$strip.border$col = "white" 
    myTheme$axis.line$col = 'white' 
    
    tiff(paste0(oDir, "/", rcp, "_", period, "_", "uncertainties_", ".tif"), width=1000, height=1200, pointsize=8, compression="lzw", res=100)
    
    print(levelplot(plot, 
                    at = zvalues, scales = list(draw=FALSE), 
                    layout=c(3, 4), 
                    xlab="", 
                    ylab="", 
                    par.settings = myTheme,
                    colorkey = myColorkey,
                    margin=FALSE)
          + layer(sp.polygons(water_mask, col="blue", lwd=0.125))
          + layer(sp.polygons(ug_mask, col="black", lwd=0.25))
          )
    dev.off()
    
    cat(paste("Processed : ", rcp, period, "\n"))
    
  }
}
