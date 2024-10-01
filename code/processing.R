library(readxl)

# read
data <- readxl::read_xlsx("/Users/82108/Downloads/raw_data.xlsx")

# processing
 
# change to  log or diff
val_df <- data.frame(
  #KOREA
  KOSPI = diff(log(data$KOSPI)),
  KRW = diff(log(data$`USD/KRW`)),
  KR3Y = diff(data$KR3Y) / head(data$KR3Y, -1),
  KR10Y = diff(data$KR10Y) / head(data$KR10Y, -1),
  
  #JAPAN 
  NIKKEI = diff(log(data$NIKKEI)),
  JPY = diff(log(data$`USD/JPY`)),
  JP3Y = diff(data$JP3Y) / head(data$JP3Y, -1),
  JP10Y = diff(data$JP10Y) / head(data$JP10Y, -1),
  
  #USA
  SP500 = diff(log(data$SP500)),
  US3Y = diff(data$US3Y) / head(data$US3Y, -1),
  US10Y = diff(data$US10Y) / head(data$US10Y, -1)
)

# na check 
for (i in 1:length(val_df)) {
  na_count <- table(is.na(val_df[, i]))
  print(na_count)
}

for (i in 1:length(val_df)) {
  na_count <- table(is.infinite(val_df[, i]))
  print(na_count)
}

# The Na and inf value is in JP3Y, JP10Y
val_df$JP3Y[is.infinite(val_df$JP3)] <- NA
val_df$JP10Y[is.infinite(val_df$JP10)] <- NA

# Drop Na values
val_df <- na.omit(val_df)

# Lastly, check it out
for (i in 1:length(val_df)) {
  na_count <- table(is.na(val_df[, i]))
  print(na_count)
}

# Append date column
date_col <- seq(from = as.Date("2010-01-05"), to = as.Date("2024-09-10"),
                       length.out = length(val_df$KOSPI))

# Total data set
val_set <- cbind(date_col, val_df)





