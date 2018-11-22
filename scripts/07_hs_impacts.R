# Heat stress impacts on pigs
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
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping" # current heat stress
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"
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

mthStats <- lapply(X = mthLs, FUN = function(mth){
  
  # load current heat stress layer
  hsC <- raster(paste0(iDir, "/outputs/30s/pig/runs/", var, "_", mth, ".tif", sep=""))
  hsC <- mask(crop(hsC, country_mask), country_mask)
  
  # load current heat stress layer
  pig_dens <- raster(paste0(iDir, "/data/pig_density/Pigs_CC2006_AD", ".tif", sep=""))
  pig_dens <- mask(crop(pig_dens, country_mask), country_mask)
  
  # calculate zonal statistics
  zonal_hs <- zonal(pig_dens, hsC, 'sum', na.rm=TRUE) 
  
  return(zonal_hs)
  
})

# merge all lists
statLs <- as.data.frame(do.call("rbind", mthStats))
statLs$sum<-round(statLs$sum) # round column
statLs$month <- rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), each = 4) #add month column
colnames(statLs) <- c("zone", "count", "month") # rename columns once again

cols <- c("red", "orange", "yellow", "grey")
labels=c("Alert","Danger","Emergency","Normal")

# grouped bar
ggplot(statLs, aes(month, count, fill=factor(zone))) +
  geom_bar(stat="identity", position = "stack", width = 0.55) +
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_fill_manual(values=cols, labels=labels) +
  xlab("") + ylab("Number of Pigs") +
  theme(legend.position="right") +
  labs(fill = "HS Categories") +
  theme_bw() +
  theme(text = element_text(size=12))

ggsave(file=paste0(oDirPlt, "/", "current_", var, "_", livestock, "_impacts", ".tiff"), 
       width = 148.5, height = 105, units = "mm")

# write output
write.csv(statLs, paste0(oDirStats, "/", "current_", var, "_", livestock, "_impacts.csv"), row.names = F)
