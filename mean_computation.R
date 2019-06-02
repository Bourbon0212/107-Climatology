setwd("E:/GitHub/107-Climatology")

middle <- c("中部二林站", "中部南投站", "中部埔里站", "中部竹山站",
            "中部大里站", "中部彰化站", "中部忠明站", "中部沙鹿站",
            "中部線西站", "中部西屯站", "中部豐原站")
north <- c("北部三重站", "北部土城站", "北部士林站", "北部大同站", 
           "北部大園站", "北部中山站", "北部中壢站", "北部古亭站", 
           "北部平鎮站", "北部永和站", "北部汐止站", "北部松山站", 
           "北部板橋站", "北部林口站", "北部桃園站", "北部基隆站", 
           "北部淡水站", "北部富貴角站", "北部菜寮站", "北部陽明站", 
           "北部新店站", "北部新莊站", "北部萬里站", "北部萬華站", 
           "北部龍潭站", "北部觀音站")
zhumiao <- c("竹苗三義站", "竹苗竹東站", "竹苗苗栗站", "竹苗湖口站", 
             "竹苗新竹站", "竹苗頭份站")
yilan <- c("宜蘭冬山站", "宜蘭宜蘭站")
huatung <- c("花東花蓮站", "花東臺東站", "花東關山站")
kaoping <- c("高屏大寮站", "高屏小港站", "高屏仁武站", "高屏左營站", 
             "高屏林園站", "高屏前金站", "高屏前鎮站", "高屏屏東站", 
             "高屏恆春站", "高屏美濃站", "高屏復興站", "高屏楠梓站", 
             "高屏鳳山站", "高屏潮州站", "高屏橋頭站")
yunchianan <- c("雲嘉南斗六站", "雲嘉南安南站", "雲嘉南朴子站", "雲嘉南崙背站", 
                "雲嘉南麥寮站", "雲嘉南善化站", "雲嘉南新港站", "雲嘉南新營站", 
                "雲嘉南嘉義站", "雲嘉南臺西站", "雲嘉南臺南站")
#lidiao <- c("離島金門站", "離島馬公站", "離島馬祖站")

all <- c(middle, north, zhumiao, yilan, huatung, kaoping, yunchianan)

#Calculate Means of Each Pollutant per Day
mean_day <- function (data) {
  #Categorize the pollutant
  data.list <- list(O3 = subset(data, 測項 == "O3")[,4:27],
                    PM10 = subset(data, 測項 == "PM10")[,4:27],
                    CO = subset(data, 測項 == "CO")[,4:27],
                    SO2 = subset(data, 測項 == "SO2")[,4:27],
                    NOx = subset(data, 測項 == "NOx")[,4:27])
  
  #Find the data length
  len = c(nrow(data.list[["O3"]]), nrow(data.list[["PM10"]]),
          nrow(data.list[["CO"]]), nrow(data.list[["SO2"]]),
          nrow(data.list[["NOx"]]))
  if (min(len) == 0) {
    len = min( len[len!=min(len)] )
  } else {
    len = min(len)
  }
  
  #Create a data.frame to store the result
  result <- data.frame("測站" = rep(data[,2][1], len))

  #1 by row, 2 by col >> by row: per day
  result[, c("O3.ppb", "PM10.μg/m3", "CO.ppb",  "SO2.ppb",  "NOx.ppb")] <- sapply(data.list, function(x) apply(x, 1, function(y) round(mean(y, na.rm = T), 3)))
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
  for (i in 1:length(all)) {
    station <- read.csv("./stations.csv")
    read_from <- paste0("./data_process/", all[i], ".csv")
    data <- read.csv(read_from, header = T)
    data$日期 <- as.Date(data$日期, format = "%Y/%m/%d")

    #Time Query
    if (query == "Jan") {
      data = filter(data, 日期 >= as.Date("2018-01-01") & 日期 <= as.Date("2018-01-31"))
    } else if (query == "Feb") {
      data = filter(data, 日期 >= as.Date("2018-02-01") & 日期 <= as.Date("2018-02-28"))
    } else if (query == "Mar") {
      data = filter(data, 日期 >= as.Date("2018-03-01") & 日期 <= as.Date("2018-03-31"))
    } else if (query == "Apr") {
      data = filter(data, 日期 >= as.Date("2018-04-01") & 日期 <= as.Date("2018-04-30"))
    } else if (query == "May") {
      data = filter(data, 日期 >= as.Date("2018-05-01") & 日期 <= as.Date("2018-05-31"))
    } else if (query == "Jun") {
      data = filter(data, 日期 >= as.Date("2018-06-01") & 日期 <= as.Date("2018-06-30"))
    } else if (query == "Jul") {
      data = filter(data, 日期 >= as.Date("2018-07-01") & 日期 <= as.Date("2018-07-31"))
    } else if (query == "Aug") {
      data = filter(data, 日期 >= as.Date("2018-08-01") & 日期 <= as.Date("2018-08-31"))
    } else if (query == "Sep") {
      data = filter(data, 日期 >= as.Date("2018-09-01") & 日期 <= as.Date("2018-09-30"))
    } else if (query == "Oct") {
      data = filter(data, 日期 >= as.Date("2018-10-01") & 日期 <= as.Date("2018-10-31"))
    } else if (query == "Nov") {
      data = filter(data, 日期 >= as.Date("2018-12-01") & 日期 <= as.Date("2018-12-31"))
    } else if (query == "Spring") {
      data = filter(data, 日期 >= as.Date("2018-03-01") & 日期 <= as.Date("2018-05-31"))
    } else if (query == "Summer") {
      data = filter(data, 日期 >= as.Date("2018-06-01") & 日期 <= as.Date("2018-08-31"))
    } else if (query == "Autumn") {
      data = filter(data, 日期 >= as.Date("2018-09-01") & 日期 <= as.Date("2018-11-30"))
    } else if (query == "Winter") {
      data1 = filter(data, 日期 >= as.Date("2018-01-01") & 日期 <= as.Date("2018-02-28"))
      data2 = filter(data, 日期 >= as.Date("2018-12-01") & 日期 <= as.Date("2018-12-31"))
      data = rbind(data1, data2)
    } else if (query == "All Year") {
      data = data
    }

    mean <- mean_data(data)
    sta_lon <- as.numeric(as.character(subset(station, 測站名稱 == mean[1])[5]))
    sta_lat <- as.numeric(as.character(subset(station, 測站名稱 == mean[1])[6]))
    result <- rbind(result, c(mean, sta_lon, sta_lat))

  }
  #Rename colnames
  result <- as.data.frame(result)
  colnames(result)[c(1,7,8)] <- c("Name", "Lon", "Lat")
  #Factor to numeric
  result[,2:8] <- lapply(result[,2:8], function(x) as.numeric(as.character(x)))
  result[,4] <- result[,4] * 10 #CO ppm > ppb
  write_to <- paste0("./result/csv/2018_", query, ".csv")
  write.csv(result, file = write_to, row.names = FALSE)
  return(result)
}

query = c("All Year", "Spring", "Summer", "Autumn", "Winter", 
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
          "Aug", "Sep", "Oct", "Nov", "Dec")

for (i in 1: length(query)) {
  mean_summary(query[i])
}