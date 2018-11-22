# Change statistics plots
# Author: John Mutua, CIAT
# Last modified: 09-11-2018

# clear your work space
rm(list = ls(all = TRUE))

# load packages
.packages = c("ggplot2", "car")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set directories
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/stats"
oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs"

var <- "hs"
livestockLs <- c("pig", "cattle")

livestock <- livestockLs[1]

#load data
rcpStats <- read.csv(paste0(iDir, "/", livestock, "_", var, "_", "change_stats.csv"))

rcpStats$RCP = recode(rcpStats$RCP, "'rcp26'='RCP 2.6'; 'rcp45'='RCP 4.5'; 'rcp60'='RCP 6.0'; 'rcp85'='RCP 8.5'")
rcpStats$PERIOD = recode(rcpStats$PERIOD, "'2020_2049'='2030s'; '2040_2069'='2050s'")
rcpStats$MONTH <- month.abb[rcpStats$MONTH] # convert month numeric to month abbreviation
rcpStats$MONTH = factor(rcpStats$MONTH, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
rcpStats$CLASS = as.factor(rcpStats$CLASS)
rcpLs <- as.character(unique(rcpStats$RCP))

# output folder
oDirPlt <- paste0(oDir, "/plots", sep="")
if (!file.exists(oDirPlt)) {dir.create(oDirPlt, recursive=T)}

barCol = c("1"="red", "2"="yellowgreen", "3"="deepskyblue4", "4"="springgreen4", "5"="orangered3", "6"="yellow4", "7"="deepskyblue", "8"="springgreen", "9"="indianred3", "10"="yellow", "11"="cyan4", "12"="wheat4", "13"="lightcoral", "14"="khaki", "15"="cyan", "16"="wheat")

for (rcp in rcpLs){
  
  rcpStats.rcp <- rcpStats[rcpStats$RCP == rcp, ]
  
  tiff(paste0(oDirPlt, "/", rcp, "_hs_change_area.tif"), width=2400, height=1200, pointsize=8, compression='lzw',res=100)
  
  f <- ggplot(data=rcpStats.rcp, aes(x=CLASS, y=AREA, fill=CLASS)) +
    theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_blank(), legend.position="none", axis.title.y = element_text(size = rel(1.2))) +
    scale_fill_manual("legend", values=barCol) +
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
