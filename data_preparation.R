setwd("E:/GitHub/107-Climatology")

middle <- c("中部二林站", "中部南投站", "中部埔里站", "中部竹山站",
            "中部大里站", "中部彰化站", "中部忠明站", "中部沙鹿站",
            "中部線西站", "中部西屯站", "中部豐原站")


for (i in 1:length(middle)) {
  #Read in data
  read_from <- paste0("./data/中部空品區/", middle[i], ".csv")
  data <- read.csv(read_from, header = F)
  data <- subset(data, V3 %in% c('O3', 'PM10', 'CO', 'SO2', 'NOx')) 
  rownames(data) <- c(1:nrow(data))
  #Factor to numeric, missing values to NAs
  data[,4:ncol(data)] <- lapply(data[,4:ncol(data)], function(x) as.numeric(as.character(x)))
  #Write to csv
  write_to <- paste0("./data_processed/中部空品區/", middle[i], ".csv")
  write.csv(data,file = write_to, row.names = FALSE)
}

#Data Integrity Check
for (i in 1:length(middle)) {
  read_from <- paste0("./data_processed/中部空品區/", middle[i], ".csv")
  data <- read.csv(read_from, header = T)
  print(as.character(data[1,2]))
  print(summary(data$V3))
}