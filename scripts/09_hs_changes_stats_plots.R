# Heat Stress changes - statistics
# Author: John Mutua, CIAT
# Last modified: 11/12/2017

# clear your work space
rm(list = ls(all = TRUE))

# load functions
source("D:/OneDrive - CGIAR/_GitHub/heat-stress-mapping/scripts/00_hs_functions.R")

# set directories
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/changes_stats"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/plots_changes"

#load data
rcpStats <- read.csv(paste0(iDir, "/hs_change_stats.csv"))

rcpStats$PERIOD = recode(rcpStats$PERIOD, "'2020_2049'='2030s'; '2040_2069'='2050s'")
rcpStats$MONTH <- month.abb[rcpStats$MONTH] # convert month numeric to month abbreviation
rcpStats$CLASS = as.factor(rcpStats$CLASS)
rcpLs <- as.character(unique(rcpStats$RCP))

if(!file.exists(oDir)){dir.create(oDir, recursive = T)}

for (rcp in rcpLs){
  
  rcpStats.rcp <- rcpStats[rcpStats$RCP == rcp, ]
  
  tiff(paste0(oDir, "/", rcp, "_hs_change_area.tif"), width=2400, height=1200, pointsize=8, compression='lzw',res=100)
  
  f <- ggplot(data=rcpStats.rcp, aes(x=CLASS, y=AREA, fill=CLASS)) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_blank(), legend.position="none", axis.title.y = element_text(size = rel(1.2))) +
    #scale_fill_manual(values=c(green2, orange, red, red2, blue2, blue3, "white")) +
    ggtitle(paste0(rcp)) +
    geom_bar(stat='identity') +
    labs(x="Change Class", y="Area")+
    theme_bw()+
    coord_flip()+
    facet_grid(PERIOD~MONTH, drop=T)+
    guides(fill=FALSE)
  
  # Plot
  print(f)
  dev.off()
  
  
}
