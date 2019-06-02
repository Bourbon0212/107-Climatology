library(ggplot2)
library(ggfortify)
library(ggmap)
library(ggtern)
library(data.table)
library(RColorBrewer)
library(sp)
library(rgdal)

All_Year <- read.csv("result/csv/2018_All Year.csv")
Spring <- read.csv("result/csv/2018_Spring.csv")
Summer <- read.csv("result/csv/2018_Summer.csv")
Autumn <- read.csv("result/csv/2018_Autumn.csv")
Winter <- read.csv("result/csv/2018_Winter.csv")
Jan <- read.csv("result/csv/2018_Jan.csv")
Feb <- read.csv("result/csv/2018_Feb.csv")
Mar <- read.csv("result/csv/2018_Mar.csv")
Apr <- read.csv("result/csv/2018_Apr.csv")
May <- read.csv("result/csv/2018_May.csv")
Jun <- read.csv("result/csv/2018_Jun.csv")
Jul <- read.csv("result/csv/2018_Jul.csv")
Aug <- read.csv("result/csv/2018_Aug.csv")
Sep <- read.csv("result/csv/2018_Sep.csv")
Oct <- read.csv("result/csv/2018_Oct.csv")
Nov <- read.csv("result/csv/2018_Nov.csv")
Dec <- read.csv("result/csv/2018_Dec.csv")

#Shapefiles >> ggplot able, TW Map as tile
TW <- readOGR(dsn = "./shp", layer = "Taiwan_county", encoding="utf8") #TWD97
TW <- spTransform(TW, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")) #WGS84
TW.f <- fortify(TW)
tw <- c(min(TW.f$long), min(TW.f$lat), max(TW.f$long), max(TW.f$lat))
Map <- get_stamenmap(tw, zoom = 8, source = "stamen", maptype = "toner-lite")

query = c("All_Year", "Spring", "Summer", "Autumn", "Winter", 
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
          "Aug", "Sep", "Oct", "Nov", "Dec")
pollut = c("O3.ppb", "PM10.μg.m3", "CO.ppm", "SO2.ppb", "NOx.ppb")

#get()讓df進去變變數；ae_string()同理
heat_map <- function(query, pollut) {
  #Plot
  heat_map <- ggmap(Map, darken = c(0.5, "white")) +
              geom_polygon(data = TW.f,
                           aes(x = long, y = lat, group = group),
                           fill = NA, col = "#00000044") +
              stat_summary_2d(data = get(query), aes_string(x = "Lon", y = "Lat", z = pollut)
                              , fun = mean, alpha = 0.6) +
              scale_fill_gradientn(name = 'Mean', colours = rev(brewer.pal(11, "RdYlGn")),
                                   limits = c(0, 50), space = 'Lab') +
              geom_point(data = get(query), aes(x = Lon, y = Lat), col = "#FFFFFF88") +
              ggtitle(paste("2018 ", query, pollut, "Pollution"))
  name = paste0(query," ", pollut, ".png")
  ggsave(filename=name, plot=heat_map)
}

for (i in 1:length(query)) {
  heat_map(query[i], "NOx.ppb")
}
