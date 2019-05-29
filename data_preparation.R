library(readxl)
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
lidiao <- c("離島金門站", "離島馬公站", "離島馬祖站")

for (i in 1:length(lidiao)) {
  #Read in data
  read_from <- paste0("./data/離島空品區/", lidiao[i], ".xls")
  data <- read_excel(read_from)
  data <- subset(data, 測項 %in% c('O3', 'PM10', 'CO', 'SO2', 'NOx')) 
  rownames(data) <- c(1:nrow(data))
  #Factor to numeric, missing values to NAs
  data[,4:ncol(data)] <- lapply(data[,4:ncol(data)], function(x) as.numeric(as.character(x)))
  #Write to csv
  write_to <- paste0("./data_processed/離島空品區/", lidiao[i], ".csv")
  write.csv(data,file = write_to, row.names = FALSE)
}

#Data Integrity Check
for (i in 1:length(lidiao)) {
  read_from <- paste0("./data_processed/離島空品區/", lidiao[i], ".csv")
  data <- read.csv(read_from, header = T)
  print(as.character(data[1,2]))
  print(summary(data$測項))
}