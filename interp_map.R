library(akima)
library(rgdal)
library(sp)
library(splancs)
library(GISTools)

#Import functions from other files
if(!exists("mean_summary", mode="function")) source("mean_computation.R")

#Shapefiles >> ggplot able, TW Map as tile
TW <- readOGR(dsn = "./shp", layer = "Taiwan_county", encoding="utf8") #TWD97
TW.mask <- poly.outer(as.points(TW@bbox[1,], TW@bbox[2,]), TW) #TWD97
TW <- spTransform(TW, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")) #WGS84
TW.mask <- spTransform(TW.mask, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")) #WGS84
TW.lim <- c(TW@bbox[1,], TW@bbox[2,])

interp_map <- function(query, pollut) {
  data <- mean_summary(query)
  
  if (pollut == "O3") {
    z.data <- data$O3.ppb
  } else if (pollut == "PM10") {
    z.data <- data$`PM10.?gg/m3`
  } else if (pollut == "CO") {
    z.data <- data$CO.ppm
  } else if (pollut == "SO2") {
    z.data <- data$SO2.ppb
  } else if (pollut == "NOx") {
    z.data <- data$NOx.ppb
  }
  
  #Remove missing values
  index <- !is.na(z.data)
  x.data <- data$Lon[index]
  y.data <- data$Lat[index]
  z.data <- z.data[index]
  
  temp <- list(x = x.data, y = y.data, z = z.data)
  img <- interp(temp[["x"]], temp[["y"]], temp[["z"]], extrap = F, linear = F)
  sta <- SpatialPoints(cbind(data[,7], data[,8]), proj4string = TW@proj4string) 
  
  png_name <- paste0(query, "_", pollut, ".png")
  
  png(filename = paste0("./result/png/", png_name))
  
  plot(TW, main = paste("2018", query, pollut, "Pollution"))
  image(img, asp = 1, add = T)
  add.masking(TW.mask, col = "white")
  points(sta, pch = 20, col = "#00000044")
  plot(TW, add = T)
  
  dev.off()
}

query = c("All Year", "Spring", "Summer", "Autumn", "Winter", 
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
          "Aug", "Sep", "Oct", "Nov", "Dec")
pollut = c("O3", "CO", "SO2", "NOx")

for (i in 1:length(query)) {
  for (j in 1:length(pollut)) {
    interp_map(query[i], pollut[j])
  }
}
interp_map("Spring", "CO")
