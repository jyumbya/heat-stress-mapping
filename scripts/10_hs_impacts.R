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
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping"
mthLS <- c(1:12)
ug_mask <- readOGR("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/admin_boundaries/UG_boundary.shp")

# create output folder
if(!dir.exists(paste0(oDir, '/outputs', sep=''))) {dir.create(paste0(oDir, '/outputs', sep=''))}

mthList <- lapply(X = mthLS, FUN = function(mth){
  
  # load current heat stress layer
  current_hs <- raster(paste0(iDir, "/outputs/raster_current/current_hs_", mth, ".tif", sep=""))
  current_hs <- mask(crop(current_hs, ug_mask), ug_mask)
  
  # load current heat stress layer
  pig_density <- raster(paste0(iDir, "/data/pig_density/Pigs_CC2006_AD", ".tif", sep=""))
  pig_density <- mask(crop(pig_density, ug_mask), ug_mask)
  
  # calculate zonal statistics
  zonal_hs <- zonal(pig_density, current_hs, 'sum', na.rm=TRUE) 
  
})

# merge all lists
statLs <- as.data.frame(do.call("rbind", mthList))
statLs$sum<-round(statLs$sum) # round column
statLs$month <- rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), each = 4) #add month column
colnames(statLs) <- c("zone", "count", "month") # rename columns once again

cols <- c("red", "orange", "yellow", "grey")
labels=c("Alert","Danger","Emergency","None")

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

ggsave(file=paste0(oDir, "/", "outputs/impacts/", "hs_current_impacts", ".tiff" ), 
       width = 148.5, height = 105, units = "mm")

# write output
write.csv(statLs, paste0(oDir, "/outputs/impacts/","hs_current_impacts.csv"), row.names = F)



