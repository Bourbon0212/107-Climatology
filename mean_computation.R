setwd("E:/GitHub/107-Climatology")

middle <- c("い场狶", "い场玭щ", "い场瓾ń", "い场λ",
            "い场ń", "い场裹て", "い场┚", "い场‵忱",
            "い场絬﹁", "い场﹁べ", "い场伦")

#Calculate Means of Each Pollutant per Day
mean_day <- function (data) {
  #Categorize the pollutant
  data.list <- list(O3 = subset(data, V3 == "O3")[,4:27],
                    PM10 = subset(data, V3 == "PM10")[,4:27],
                    CO = subset(data, V3 == "CO")[,4:27],
                    SO2 = subset(data, V3 == "SO2")[,4:27],
                    NOx = subset(data, V3 == "NOx")[,4:27])

  #Create a data.frame to store the result
  result <- data.frame("代" = rep(data[,"V2"][1], nrow(data)/5))

  #1 by row, 2 by col >> by row: per day
  result[, c("O3.ppb", "PM10.g/m3", "CO.ppm",  "SO2.ppb",  "NOx.ppb")] <- sapply(data.list, function(x) apply(x, 1, function(y) round(mean(y, na.rm = T), 3)))
  return(result)
}

#Calculate Means of Each Pollutant per Station
mean_data <- function(data) {
  data <- mean_day(data)
  result <- c(as.character(data[1,1]))
  #1 by row, 2 by col >> by col: each pollutant's mean
  result <- c(result, apply(data[,2:6], 2, function(x) round(mean(x, na.rm = T), 3)))
  return(result)
}

library(dplyr)
library(lubridate)
#Combine Means of All Stations
mean_summary <- function(query) {
  result <- vector()
  #Combine means with their lon, lat
  for (i in 1:length(middle)) {
    station <- read.csv("./data_processed/stations.csv")
    read_from <- paste0("./data_processed/い场珇跋/", middle[i], ".csv")
    data <- read.csv(read_from, header = T)
    data$V1 <- as.Date(data$V1, format = "%Y/%m/%d")
    
    #Time Query
    if (query == "Jan") {
      data = filter(data, V1 >= as.Date("2018-01-01") & V1 <= as.Date("2018-01-31"))
    } else if (query == "Feb") {
      data = filter(data, V1 >= as.Date("2018-02-01") & V1 <= as.Date("2018-02-28"))
    } else if (query == "Mar") {
      data = filter(data, V1 >= as.Date("2018-03-01") & V1 <= as.Date("2018-03-31"))
    } else if (query == "Apr") {
      data = filter(data, V1 >= as.Date("2018-04-01") & V1 <= as.Date("2018-04-31"))
    } else if (query == "May") {
      data = filter(data, V1 >= as.Date("2018-05-01") & V1 <= as.Date("2018-05-31"))
    } else if (query == "Jun") {
      data = filter(data, V1 >= as.Date("2018-06-01") & V1 <= as.Date("2018-06-30"))
    } else if (query == "Jul") {
      data = filter(data, V1 >= as.Date("2018-07-01") & V1 <= as.Date("2018-07-31"))
    } else if (query == "Aug") {
      data = filter(data, V1 >= as.Date("2018-08-01") & V1 <= as.Date("2018-08-31"))
    } else if (query == "Sep") {
      data = filter(data, V1 >= as.Date("2018-09-01") & V1 <= as.Date("2018-09-30"))
    } else if (query == "Oct") {
      data = filter(data, V1 >= as.Date("2018-10-01") & V1 <= as.Date("2018-10-31"))
    } else if (query == "Nov") {
      data = filter(data, V1 >= as.Date("2018-12-01") & V1 <= as.Date("2018-12-31"))
    } else if (query == "Spring") {
      data = filter(data, V1 >= as.Date("2018-03-01") & V1 <= as.Date("2018-05-31"))
    } else if (query == "Summer") {
      data = filter(data, V1 >= as.Date("2018-06-01") & V1 <= as.Date("2018-08-31"))
    } else if (query == "Autumn") {
      data = filter(data, V1 >= as.Date("2018-09-01") & V1 <= as.Date("2018-11-30"))
    } else if (query == "Winter") {
      data1 = filter(data, V1 >= as.Date("2018-01-01") & V1 <= as.Date("2018-02-28"))
      data2 = filter(data, V1 >= as.Date("2018-12-01") & V1 <= as.Date("2018-12-31"))
      data = rbind(data1, data2)
    } else if (query == "All Year") {
      data = data
    }
    
    mean <- mean_data(data)
    sta_lon <- as.numeric(as.character(subset(station, 代嘿 == mean[1])[5]))
    sta_lat <- as.numeric(as.character(subset(station, 代嘿 == mean[1])[6]))
    result <- rbind(result, c(mean, sta_lon, sta_lat))
    
  }
  #Rename colnames
  result <- as.data.frame(result)
  colnames(result)[c(1,7,8)] <- c("Name", "Lon", "Lat")
  #Factor to numeric
  result[,2:8] <- lapply(result[,2:8], function(x) as.numeric(as.character(x)))
  
  return(result)
}
