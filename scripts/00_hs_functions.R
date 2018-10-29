## functions for calculating heat stress for different livestock species
## j.mutua, CIAT

# heat stress index function 
# y=relative humidity; x=maximum temperature
pig.index<-function(x,y) {
  x<-x[]
  y<-y[]
  z<-0
  for(i in 1:length(x)){
    z[i] <- if(is.na(x[i])|is.na(y[i])){
      NA
    }else if(y[i] < 45 & x[i] >= 31.0 | 
             y[i] >= 45 & y[i] < 55 & x[i] >= 30.0 | 
             y[i] >= 55 & y[i] < 75 & x[i] >= 29.0 | 
             y[i] >= 75 & y[i] < 90 & x[i] >= 29.0 | 
             y[i] >= 90 & x[i] >= 27.0){
      1
    }else if(y[i] < 45 & x[i] >= 28.0 & x[i] < 31.0 | 
             y[i] >= 45 & y[i] < 55 & x[i] >= 27.0 & x[i] < 30.0 | 
             y[i] >= 55 & y[i] < 75 & x[i] >= 27.0 & x[i] < 29.0 | 
             y[i] >= 75 & y[i] < 90 & x[i] >= 26.0 & x[i] < 29.0 | 
             y[i] >= 90 & x[i] >= 26.0 & x[i] < 27.0){
      2
    }else if(y[i] < 45 & x[i] >= 26.0 & x[i] < 28.0 | 
             y[i] >= 45 & y[i] < 55 & x[i] >= 25.0 & x[i] < 27.0 | 
             y[i] >= 55 & y[i] < 75 & x[i] >= 25.0 & x[i] < 27.0 | 
             y[i] >= 75 & y[i] < 90 & x[i] >= 24.0 & x[i] < 26.0 | 
             y[i] >= 90 & x[i] >= 24.0 & x[i] < 26.0){
      3
    }else if(y[i] < 45 & x[i] < 26.0 | 
             y[i] >= 45 & y[i] < 55 & x[i] < 25.0 | 
             y[i] >= 55 & y[i] < 75 & x[i] < 25.0 | 
             y[i] >= 75 & y[i] < 90 & x[i] < 24.0 | 
             y[i] >= 90 & x[i] < 24.0){
      4}else{NA}
  }
  return(z)
}

# heat stress index function 
# y=relative humidity; x=maximum temperature
cattle.index<-function(x,y) {
  x<-x[]
  y<-y[]
  z<-0
  for(i in 1:length(x)){
    
    x <- x*1.8+32 # convert celsius to fahreinheit
    z <- x-(0.55-(0.55*y/100))*(x-58) # cattle heat stress formulae
    
  }
  return(z)
}

#calculate difference maps
hs.change <- function(x,y) {
  x<-x[]
  y<-y[]
  z<-0
  for(i in 1:length(x)){
    z[i] <- if(is.na(x[i])|is.na(y[i])){ 
      NA
    }else if(x[i] == 1 & y[i] == 1){ 1 #HS alert to HS alert
    }else if(x[i] == 1 & y[i] == 2){ 2 #HS alert to HS danger
    }else if(x[i] == 1 & y[i] == 3){ 3 #HS alert to HS emergency
    }else if(x[i] == 1 & y[i] == 4){ 4 #HS alert to No HS
    }else if(x[i] == 2 & y[i] == 1){ 5 #HS danger to HS alert
    }else if(x[i] == 2 & y[i] == 2){ 6 #HS danger to HS danger
    }else if(x[i] == 2 & y[i] == 3){ 7 #HS danger to HS emergency
    }else if(x[i] == 2 & y[i] == 4){ 8 #HS danger to No HS
    }else if(x[i] == 3 & y[i] == 1){ 9 #HS emergency to HS alert
    }else if(x[i] == 3 & y[i] == 2){ 10 #HS emergency to HS danger
    }else if(x[i] == 3 & y[i] == 3){ 11 #HS emergency to HS emergency
    }else if(x[i] == 3 & y[i] == 4){ 12 #HS emergency to No HS
    }else if(x[i] == 4 & y[i] == 1){ 13 #No HS to HS alert
    }else if(x[i] == 4 & y[i] == 2){ 14 #No HS to HS danger
    }else if(x[i] == 4 & y[i] == 3){ 15 #No HS to HS emergency
    }else if(x[i] == 4 & y[i] == 4){ 16} #No HS to No HS
    else{NA}
    }
  return(z)
}

# calculate everything per pixel
categorical.mode <- function(pixel){
  mode.pixel <- DescTools::Mode(x = pixel)[1]
  return(mode.pixel)
}

categorical.entropy <- function(pixel){
  entropy.pixel <- DescTools::Entropy(x = table(pixel), base = exp(1))/DescTools::Entropy(x = rep(length(pixel)/length(table(pixel)), length(table(pixel))), base = exp(1))
  return(entropy.pixel)
}


#calculate change area
hs.change.stats <- function(x){
  
  #initial 16 classes spread out
  classes <- 1:16
  
  #calculate areas
  b <- getValues(area(x, weights=FALSE))
  
  #aggregate areas by group and sum them up
  b <- aggregate(b, by=list(getValues(x)), sum, na.rm=T)
  b <- as.data.frame(b)
  
  #change column names
  names(b) <- c("CLASS","AREA")
  
  #bring in classes with NAs
  b <- merge(data.frame(CLASS = classes), b, by='CLASS', all.x=T)
  
  return(b)
  
}