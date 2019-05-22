setwd("E:/GitHub/107-Climatology")

middle <- c("い场狶", "い场玭щ", "い场瓾ń", "い场λ",
            "い场ń", "い场裹て", "い场┚", "い场‵忱",
            "い场絬﹁", "い场﹁べ", "い场伦")


for (i in 1:length(middle)) {
  #Read in data
  read_from <- paste0("./data/い场珇跋/", middle[i], ".csv")
  data <- read.csv(read_from, header = F)
  data <- subset(data, V3 %in% c('O3', 'PM10', 'CO', 'SO2', 'NOx')) 
  rownames(data) <- c(1:nrow(data))
  #Factor to numeric, missing values to NAs
  data[,4:ncol(data)] <- lapply(data[,4:ncol(data)], function(x) as.numeric(as.character(x)))
  #Write to csv
  write_to <- paste0("./data_processed/い场珇跋/", middle[i], ".csv")
  write.csv(data,file = write_to, row.names = FALSE)
}

#Data Integrity Check
for (i in 1:length(middle)) {
  read_from <- paste0("./data_processed/い场珇跋/", middle[i], ".csv")
  data <- read.csv(read_from, header = T)
  print(as.character(data[1,2]))
  print(summary(data$V3))
}