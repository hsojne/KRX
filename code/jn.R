library(devtools)
library(ConnectednessApproach)
library(dplyr)
library(quantmod)
library(readxl)
library(zoo)
library(ggplot2)
library(tseries)
library(moments)
#----------------------------------------------

data <- readxl::read_xlsx("/Users/82108/Downloads/raw_data.xlsx")


par(mfrow = c(2, 2))
plot(data$date, data$KOSPI, col ="#003366", type ="l", lwd = 0.5, xlab = NA, ylab = NA, main = "KOSPI",font.main = 1)
plot(data$date, data$KR3Y, col ="#003366",  type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "KR3Y",font.main = 1)
plot(data$date, data$KR10Y, col ="#003366", type ="l", lwd = 0.5, xlab = NA, ylab = NA, main = "KR10Y",font.main = 1)
plot(data$date, data$`USD/KRW`, col ="#003366",  type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "USD/KRW",font.main = 1)


plot(data$date, data$NIKKEI, col ="#003300", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "NIKKEI",font.main = 1)
plot(data$date, data$JP3Y, col ="#003300", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "JP3Y",font.main = 1)
plot(data$date, data$JP10Y, col ="#003300",type ="l", lwd = 0.5, xlab = NA, ylab = NA, main = "JP10Y",font.main = 1)
plot(data$date, data$`USD/JPY`, col ="#003300", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "USD/JPY",font.main = 1)


plot(data$date, data$SP500, col ="#000033", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "SP500",font.main = 1)
plot(data$date, data$US3Y, col ="#000033", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "US3Y",font.main = 1)
plot(data$date, data$US10Y, col ="#000033", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "US10Y",font.main = 1)
#----------------------------------------------
adf.test(data$US10Y)

test_set <- as.matrix(data[c(-1)])
apply(test_set, FUN = log,MARGIN = 2)

test_set[,"JP10Y"] <- test_set[,"JP10Y"] + 0.001
test_set[, "JP3Y"] <- test_set[, "JP3Y"] + 0.001

test_df <- as.data.frame(test_set)


quantile(log_data$KOSPI_diff, 0.05)
quantile(log_data$KOSPI_diff, 0.95)
#----------------------------------------------


# 로그 차분을 적용한 후, 새로운 데이터프레임 생성
log_data <- data.frame(
  KOSPI_diff = diff(log(test_df$KOSPI)),          # KOSPI 로그 차분
  USD_KRW_diff = diff(log(test_df$`USD/KRW`)),    # USD/KRW 로그 차분
  KR3Y_diff = diff(log(test_df$KR3Y)),            # KR3Y 로그 차분
  KR10Y_diff = diff(log(test_df$KR10Y)),          # KR10Y 로그 차분
  NIKKEI_diff = diff(log(test_df$NIKKEI)),        # NIKKEI 로그 차분
  USD_JPY_diff = diff(log(test_df$`USD/JPY`)),    # USD/JPY 로그 차분
  JP3Y_diff = diff(log(test_df$JP3Y)),            # JP3Y 로그 차분
  JP10Y_diff = diff(log(test_df$JP10Y)),          # JP10Y 로그 차분
  SP500_diff = diff(log(test_df$SP500)),          # SP500 로그 차분
  US3Y_diff = diff(log(test_df$US3Y)),            # US3Y 로그 차분
  US10Y_diff = diff(log(test_df$US10Y))           # US10Y 로그 차분
)

date_set <- data$date[c(-1)]

#----------------------------------------------

round(apply(log_data, FUN = mean,MARGIN = 2), 5)
round(apply(log_data, FUN = max,MARGIN = 2), 5)
round(apply(log_data, FUN = min,MARGIN = 2), 5)
round(apply(log_data, FUN = sd,MARGIN = 2), 5)
round(apply(log_data, FUN = skewness,MARGIN = 2), 5)
round(apply(log_data, FUN = kurtosis,MARGIN = 2), 5)

jarque.bera.test(log_data$US10Y_diff)
#----------------------------------------------

par(mfrow = c(2, 2))
plot(date_set,log_data$KOSPI_diff, col ="#003366", type ="l", lwd = 0.5, xlab = NA, ylab = NA, main = "KOSPI",font.main = 1)
plot(date_set,log_data$USD_KRW_diff, col ="#003366",  type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "KR3Y",font.main = 1)
plot(date_set,log_data$KR3Y_diff, col ="#003366", type ="l", lwd = 0.5, xlab = NA, ylab = NA, main = "KR10Y",font.main = 1)
plot(date_set,log_data$KR10Y_diff, col ="#003366",  type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "USD/KRW",font.main = 1)


plot(date_set,log_data$NIKKEI_diff, col ="#003300", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "NIKKEI",font.main = 1)
plot(date_set,log_data$USD_JPY_diff, col ="#003300", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "JP3Y",font.main = 1)
plot(date_set,log_data$JP3Y_diff, col ="#003300",type ="l", lwd = 0.5, xlab = NA, ylab = NA, main = "JP10Y",font.main = 1)
plot(date_set,log_data$JP10Y_diff, col ="#003300", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "USD/JPY",font.main = 1)


plot(date_set,log_data$SP500_diff, col ="#000033", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "SP500",font.main = 1)
plot(date_set,log_data$US3Y_diff, col ="#000033", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "US3Y",font.main = 1)
plot(date_set,log_data$US10Y_diff, col ="#000033", type ="l",lwd = 0.5, xlab = NA, ylab = NA, main = "US10Y",font.main = 1)

#----------------------------------------------
writexl::write_xlsx(log_data, "/Users/82108/Desktop/log_data.xlsx")

# 

V_INDEX <- diff[val]*W(0.05) + diff[val]*W(0.5) + diff[val]*W(0.95)




W <- 




