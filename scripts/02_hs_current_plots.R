# Heat Stress current plots
# Author: John Mutua, CIAT
# Last modified: 18/09/2017

# clear your work space
rm(list = ls(all = TRUE))

# load packages
.packages = c("rasterVis","maptools","sf", "rgdal")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/raster_current"
var <- "current_hs"
id <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# import country mask
ug_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")

# import water mask
water_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/other/ug_waterbodies.shp")

pDir <- paste0("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs", "/plots_current")
if (!file.exists(pDir)) {dir.create(pDir, recursive=T)}

var_stk <- stack(paste0(iDir, "/", var, "_", 1:12, ".tif"))
var_stk_msk <- mask(crop(var_stk, ug_mask), ug_mask)

if (var=="current_hs"){
  breaks <- 5
  zvalues <- seq(min(minValue(var_stk_msk)), max(maxValue(var_stk_msk)), length.out = breaks) # define limits
  colors <- c("red", "orange", "yellow", "grey")
  myColorkey <- list(at=zvalues,height=0.95, width=1.5, labels=list(at=zvalues+0.5,labels=c("HS alert","HS danger","HS emergency","No HS"), cex=1.25))
} else {"error"}

plot <- setZ(var_stk_msk, id)
names(plot) <- id

myTheme <- BuRdTheme() 
myTheme$regions$col = colorRampPalette(colors)(length(zvalues)-1)
myTheme$strip.border$col = "white" 
myTheme$axis.line$col = 'white' 

tiff(paste0(pDir, "/", var, ".tif"), width=1000, height=1200, pointsize=8, compression="lzw", res=100)

print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), 
                layout=c(3, 4), 
                xlab="", 
                ylab="", 
                par.settings = myTheme, 
                colorkey = myColorkey,
                margin=FALSE) 
      + layer(sp.polygons(water_mask, col="blue", lwd=0.125))
      + layer(sp.polygons(ug_mask, col="black", lwd=0.25)))

dev.off()

