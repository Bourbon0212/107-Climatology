library(vcd)
library(ggplot2)
library(RColorBrewer)

# Data
All_Year <- read.csv("result/csv/2018_All Year.csv")
index <- c(c(1:11), c(13, 14), c(16:47), c(49:74))
O3 <- All_Year$O3.ppb[index]
PM10 <- All_Year$PM10.£gg.m3[index]
CO <- All_Year$CO.ppm[index] * 10
SO2 <- All_Year$SO2.ppb[index]
NOx <- All_Year$NOx.ppb[index]
station <- All_Year$Name[index]

test <- matrix(c(O3, PM10, CO, SO2, NOx), ncol = 5, byrow = F)
colnames(test) <- c("O3", "PM10", "CO", "SO2", "NOx")
rownames(test) <- station

# mosaic
mosaic(test, shade = T, color = T, labeling = labeling_border(rot_labels = c(90, 90, 0, 0)))
