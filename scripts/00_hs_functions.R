## Functions for calculating heat stress for different livestock species

### PIG HEAT STRESS ###

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


#calculate difference maps
pig.index.change <- function(x,y) {
  x<-x[]
  y<-y[]
  z<-0
  for(i in 1:length(x)){
    z[i] <- if(is.na(x[i])|is.na(y[i])){ 
      NA
    }else if(x[i] == 1 & y[i] == 1){ 1 #Emergency to Emergency
    }else if(x[i] == 1 & y[i] == 2){ 2 #Emergency to Danger
    }else if(x[i] == 1 & y[i] == 3){ 3 #Emergency to Alert
    }else if(x[i] == 1 & y[i] == 4){ 4 #Emergency to None
    }else if(x[i] == 2 & y[i] == 1){ 5 #Danger to Emergency
    }else if(x[i] == 2 & y[i] == 2){ 6 #Danger to Danger
    }else if(x[i] == 2 & y[i] == 3){ 7 #Danger to Alert
    }else if(x[i] == 2 & y[i] == 4){ 8 #Danger to None
    }else if(x[i] == 3 & y[i] == 1){ 9 #Alert to Emergency
    }else if(x[i] == 3 & y[i] == 2){ 10 #Alert to Danger
    }else if(x[i] == 3 & y[i] == 3){ 11 #Alert to Alert
    }else if(x[i] == 3 & y[i] == 4){ 12 #Alert to None
    }else if(x[i] == 4 & y[i] == 1){ 13 #None to Emergency
    }else if(x[i] == 4 & y[i] == 2){ 14 #None to Danger
    }else if(x[i] == 4 & y[i] == 3){ 15 #None to Alert
    }else if(x[i] == 4 & y[i] == 4){ 16} #None to None
    else{NA}
    }
  return(z)
}

#pig index change matrix
pig.index.change.matrix <- function(x,y){
  
  #assign colors
  cols <- c("red", "yellowgreen", "deepskyblue4", "springgreen4", 
            "orangered3", "yellow4", "deepskyblue", "springgreen", 
            "indianred3", "yellow", "cyan4", "wheat4", 
            "lightcoral", "khaki", "cyan", "wheat")
  
  #create matrix
  m4<- matrix(1:16, nrow=4, ncol=4, byrow=TRUE)
  
  rownames(m4) <- c("Emergency","Danger","Alert","None") 
  colnames(m4) <- c("Emergency","Danger","Alert","None") 
  
  image(1:nrow(m4), 1:ncol(m4), m4, col=cols, axes=FALSE, xlab="Current", ylab="Future")
  axis(1, at = 1:ncol(m4), labels=colnames(m4), tick=T)
  axis(2, at = 1:nrow(m4), labels=rownames(m4), tick=T)
  box()
  
}

#calculate change area
pig.index.change.stats <- function(x){
  
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

#calculate heat stress zone area
pig.index.zone.stats <- function(x){
  
  #initial 4 classes spread out
  classes <- 1:4
  
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

###-----------------------------------------------------###
### CATTLE HEAT STRESS ###

#cattle heat stress index
#y=relative humidity; x=maximum temperature
cattle.index <- function(x,y){
  x <- x*1.8+32 # convert celsius to fahreinheit
  z <- x-(0.55-(0.55*y/100))*(x-58) # cattle heat stress formulae
  return(z)
}

#classify cattle heat stress index
#1=critical; 2=emergency; 3=danger; 4=alert; 5=None
cattle.index.class <- function(x){
  ifelse(x<75, 5, 
         ifelse(x >= 75 & x <= 78, 4, 
                ifelse(x > 78 & x <= 83, 3, 
                       ifelse(x > 83 & x <= 99, 2, 
                              ifelse(x > 99, 1, NA)))))
}

#calculate difference maps
cattle.index.change <- function(x,y) {
  x<-x[]
  y<-y[]
  z<-0
  for(i in 1:length(x)){
    z[i] <- if(is.na(x[i])|is.na(y[i])){ 
      NA
    }else if(x[i] == 1 & y[i] == 1){ 1 #critical to critical
    }else if(x[i] == 1 & y[i] == 2){ 2 #critical to emergency
    }else if(x[i] == 1 & y[i] == 3){ 3 #critical to danger
    }else if(x[i] == 1 & y[i] == 4){ 4 #critical to alert
    }else if(x[i] == 1 & y[i] == 5){ 5 #critical to none
    }else if(x[i] == 2 & y[i] == 1){ 6 #emergency to critical
    }else if(x[i] == 2 & y[i] == 2){ 7 #emergency to emergency
    }else if(x[i] == 2 & y[i] == 3){ 8 #emergency to danger
    }else if(x[i] == 2 & y[i] == 4){ 9 #emergency to alert
    }else if(x[i] == 2 & y[i] == 5){ 10 #emergency to none
    }else if(x[i] == 3 & y[i] == 1){ 11 #danger to critical
    }else if(x[i] == 3 & y[i] == 2){ 12 #danger to emergency
    }else if(x[i] == 3 & y[i] == 3){ 13 #danger to danger
    }else if(x[i] == 3 & y[i] == 4){ 14 #danger to alert
    }else if(x[i] == 3 & y[i] == 5){ 15 #danger to none
    }else if(x[i] == 4 & y[i] == 1){ 16 #alert to critical
    }else if(x[i] == 4 & y[i] == 2){ 17 #alert to emergency
    }else if(x[i] == 4 & y[i] == 3){ 18 #alert to danger
    }else if(x[i] == 4 & y[i] == 4){ 19 #alert to alert
    }else if(x[i] == 4 & y[i] == 5){ 20 #alert to none
    }else if(x[i] == 5 & y[i] == 1){ 21 #none to critical
    }else if(x[i] == 5 & y[i] == 2){ 22 #none to emergency
    }else if(x[i] == 5 & y[i] == 3){ 23 #none to danger
    }else if(x[i] == 5 & y[i] == 4){ 24 #none to alert
    }else if(x[i] == 5 & y[i] == 5){ 25} #none to none
    else{NA}
  }
  return(z)
}

#cattle heat stress change area
cattle.index.change.stats <- function(x){
  
  #initial 25 classes spread out
  classes <- 1:25
  
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

#cattle heat stress change area
cattle.index.current.stats <- function(x){
  
  #initial 5 classes spread out
  classes <- 1:5
  
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
  

# calculate everything per pixel
categorical.mode <- function(pixel){
  mode.pixel <- DescTools::Mode(x = pixel)[1]
  return(mode.pixel)
}

agreement.pixel <- function(pixel) {
  x <- pixel[1]
  y <- pixel[2:length(pixel)]
  length(y[y == x])
}


categorical.entropy <- function(pixel){
  entropy.pixel <- DescTools::Entropy(x = table(pixel), base = exp(1))/DescTools::Entropy(x = rep(length(pixel)/length(table(pixel)), length(table(pixel))), base = exp(1))
  return(entropy.pixel)
}