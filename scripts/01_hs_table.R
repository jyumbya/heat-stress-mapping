setwd("D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/climate_data")

#set variables
iDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/data/climate_data"

#read data
sites <- read.csv(paste0(iDir, "/", "samples14082019_observ_january_2010.csv"), header=TRUE)

sites$hs_daily <- 0

for(i in 1:nrow(sites)){
  
  #site_row <- sites[65,]
  
  site_row<- sites[i,]
  
  y <- site_row$relativeHumidity.avg
  x <- site_row$temperatures.max
  
  hs_value <- pig.index(x, y)
  
  sites$hs_daily[i] <- hs_value
  
}

#write to csv
write.csv(sites , file = paste0(iDir, "/", "samples14082019_daily_hs_january_2010.csv"), row.names = FALSE)

