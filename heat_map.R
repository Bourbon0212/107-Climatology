library(ggplot2)
library(ggfortify)
library(ggmap)
library(ggtern)
library(data.table)
library(sp)
library(rgdal)

#Shapefiles >> ggplot able, TW Map as tile
TW <- readOGR(dsn = "./shp", layer = "Taiwan_county", encoding="utf8") #TWD97
TW <- spTransform(TW, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")) #WGS84
TW.f <- fortify(TW)
tw <- c(min(TW.f$long), min(TW.f$lat), max(TW.f$long), max(TW.f$lat))
Map <- get_stamenmap(tw, zoom = 8, source = "stamen", maptype = "toner-lite")

heat_map <- function(query, pollut) {
  #Import functions from other files
  if(!exists("mean_summary", mode="function")) source("mean_computation.R")
  
  #Mean_summary result 
  result <- mean_summary(query)
  if (pollut == "O3") {
    index <- result$O3.ppb / 10
  } else if (pollut == "PM10") {
    index <- result$`PM10.£gg/m3` / 10
  } else if (pollut == "CO") {
    index <- result$CO.ppm * 10
  } else if (pollut == "SO2") {
    index <- result$SO2.ppb
  } else if (pollut == "NOx") {
    index <- result$NOx.ppb / 10
  }
  
  data.plot <- data.table(result)[,list(x=rep(Lon, index),y=rep(Lat, index))]
  
  #Plot
  heat_map <- ggmap(Map, darken = c(0.5, "white")) +
              geom_polygon(data = TW.f,
                           aes(x = long, y = lat, group = group),
                           fill = NA, col = "#00000044") +
              stat_density_2d(data = data.plot, aes(x = x, y = y, fill = ..level..), 
                              geom = "polygon", alpha = .5, color = NA) +
              scale_fill_gradient2("Pollution\nLevel", low = "white", mid = "yellow", high = "red") +
              geom_point(data = result, aes(x = Lon, y = Lat), col = "#FFFFFF44") +
              ggtitle(paste("2018", query, pollut, "Pollution")) +
              theme(legend.text = element_blank())
  print(heat_map)
}