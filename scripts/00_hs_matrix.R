# change matrix plot

#clear your work space
rm(list = ls(all = TRUE))

oDir <- "D:/OneDrive - CGIAR/Projects/2018/L&E_Heat_Stress_Mapping/outputs/"

require(grDevices)

#assign colors
cols <- c("red", "yellowgreen", "deepskyblue4", "springgreen4", "orangered3", "yellow4", "deepskyblue", 
          "springgreen", "indianred3", "yellow", "cyan4", "wheat4", "lightcoral", "khaki", "cyan", "wheat")

#create matrix
m4 <- matrix(c(1,2,3,4,
               5,6,7,8,
               9,10,11,12,
               13,14,15,16), nrow = 4, ncol = 4, byrow = TRUE)

rownames(m4) <- c("Alert","Danger","Emergency","No") 
colnames(m4) <- c("Alert","Danger","Emergency","No") 

#plot matrix
tiff(paste0(oDir, "/change_matrix.tiff"), width=600, height=600, compression='lzw',res=100)
image(1:nrow(m4), 1:ncol(m4), m4, col=cols, axes=FALSE, xlab="Future", ylab="Current")
axis(1, at = 1:ncol(m4), labels=colnames(m4), tick=T)
axis(2, at = 1:nrow(m4), labels=rownames(m4), tick=T)
box()
# title(main = "Heat Stress Change Transitions", font.main = 4)
dev.off()