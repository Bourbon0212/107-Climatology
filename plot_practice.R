library(rgdal)
library(sp)
library(splancs)
library(GISTools)
library(ggplot2)
library(ggfortify)
library(ggmap)
library(ggtern)
library(data.table)

#Import functions from other files
if(!exists("mean_summary", mode="function")) source("mean_computation.R")

#Shapefiles >> ggplot able, TW Map as tile
TW <- readOGR(dsn = "./shp", layer = "Taiwan_county", encoding="utf8") #TWD97
#TW.mask <- poly.outer(as.points(TW@bbox[1,], TW@bbox[2,]), TW) #TWD97

TW <- spTransform(TW, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")) #WGS84
#TW.mask <- spTransform(TW.mask, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")) #WGS84

TW.lim <- c(TW@bbox[1,], TW@bbox[2,])
TW.f <- fortify(TW)
tw <- c(min(TW.f$long), min(TW.f$lat), max(TW.f$long), max(TW.f$lat))
Map <- get_stamenmap(tw, zoom = 8, source = "stamen", maptype = "toner-lite")

#Mean_summary result 
result <- mean_summary("Winter")

#ggtern method
kde.pts <- kde2d.weighted(result$Lon, result$Lat, 0.5, 100, TW.lim, result$O3.ppb)
image(kde.pts, asp = 1) #Remain x/y ratio
add.masking(TW.mask, col = "white")
plot(TW, add = T)

data.plot <- data.table(result)[,list(x=rep(Lon,result$CO.ppm  *10),y=rep(Lat,result$CO.ppm *10))]

#Plot
ggmap(Map, darken = c(0.5, "white")) +
  geom_polygon(data = TW.f,
               aes(x = long, y = lat, group = group),
               fill = NA, col = "#00000044") +
  stat_density_2d(data = data.plot, aes(x = x, y = y, fill = ..level..), 
                  geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Pollutant", low = "white", mid = "yellow", high = "red")