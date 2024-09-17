library(devtools)
library(ConnectednessApproach)
library(dplyr)
library(quantmod)
library(fredr)
library(readxl)
library(zoo)
library(ggplot2)

fredr_set_key("89104d906090e4319bbd61e6d862b4f0")
#--------------------------------------------------------
## data of korea
kospi_raw <- quantmod::getSymbols("^KS11", from="1999-01-01",
                              to="2024-09-11", auto.assign=F)
core_kospi <- coredata(kospi_raw)
KOSPI <- data.frame(
   date = index(kospi_raw),kospi = core_kospi[, 4]
)

korea_ex_raw <- fredr(
    series_id = "DEXKOUS",
    observation_start = as.Date("1990-01-01"),
    observation_end = as.Date("2024-09-11"))
korea_ex <- data.frame(
  date = korea_ex_raw$date, korea_ex = korea_ex_raw$value
)

korea_3_years <- read.csv("Desktop/korea_3.csv")
colnames(korea_3_years) <- c("date", "3_bond_ko")
korea_3_years$date <- as.Date(korea_3_years$date)
korea_3_years <- na.omit(korea_3_years)

korea_10_years <- read.csv("Desktop/korea_10.csv")
colnames(korea_10_years) <- c("date", "10_bond_ko")
korea_10_years$date <- as.Date(korea_10_years$date)
korea_10_years <- na.omit(korea_10_years)

total_korea_raw <- Reduce(function(x, y) full_join(x, y), list(KOSPI, korea_ex,
                                                           korea_3_years, korea_10_years))
total_korea <- na.omit(total_korea_raw)
#--------------------------------------------------------
## data of usa
sp500_raw <- getSymbols("^GSPC", from="1999-01-01",
                    to="2024-09-11", auto.assign=F)

core_sp500 <- coredata(sp500_raw)
SP500 <- data.frame(
  date = index(sp500_raw), sp500 = core_sp500[, 4]
)

usa_3_years_raw <- fredr(
  series_id = "DGS3",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2024-09-11"))

usa_3_years <- data.frame(
  date = usa_3_years_raw$date, bond_us_3 = usa_3_years_raw$value
)

usa_10_years <- read.csv("Desktop/usa_1 0.csv")
colnames(usa_10_years) <- c("date", "bond_us_10")
usa_10_years$date <- as.Date(usa_10_years$date)

total_us_raw <- Reduce(function(x, y) full_join(x, y), list(SP500,
                                                            usa_3_years, usa_10_years))
total_us <- na.omit(total_us_raw)
#--------------------------------------------------------
## data of japan
nikkei_raw <- getSymbols("^N225", from="1999-01-01",
                     to="2024-09-11", auto.assign=F)
core_nikkei <- coredata(nikkei_raw)
NIKKEI <- data.frame(
  date = index(nikkei), nikkei = core_nikkei[, 4]
)

japan_ex_raw <- fredr(
  series_id = "DEXJPUS",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2024-09-11"))

japan_ex <- data.frame(
  date = japan_ex_raw$date, japan_ex = japan_ex_raw$value
)

japan_10_years_raw <- read_xlsx("Desktop/주요국 국채 금리.xlsx", sheet = 1)
colnames(japan_10_years_raw) <- c("date", "bond_jp_10")
japan_10_years_raw$date <- as.Date(japan_10_years_raw$date)

japan_3_years_raw <- read_xlsx("Desktop/주요국 국채 금리.xlsx", sheet = 2)
colnames(japan_3_years_raw) <- c("date", "bond_jp_3")
japan_3_years_raw$date <- as.Date(japan_3_years_raw$date)

total_jp_raw <- Reduce(function(x,y) full_join(x,y), list(NIKKEI,
                                                          japan_ex, japan_10_years_raw,
                                                          japan_3_years_raw))
total_jp <- na.omit(total_jp_raw)


data_set_raw <- Reduce(function(x,y) full_join(x,y),
                   list(total_korea, total_jp, total_us))
data_set <- na.omit(data_set_raw)
colnames(data_set) <- c("date", "KOSPI", "USD/KRW", "KR3Y", "KR10Y",
                        "NIKKEI", "USD/JPY", "JP10Y", "JP3Y",
                        "SP500", "US3Y", "US10Y")


new_data <- data.frame(matrix(ncol = ncol(data_set), nrow = nrow(data_set) ))
for (i in 2:length(data_set)) {
  new_data[, i]<- data_set[, i]/lag(data_set[, i]) - 1
}


log_data <- new_data[c(-1)]
colnames(log_data) <- c("KOSPI", "USD/KRW", "KR3Y", "KR10Y",
                        "NIKKEI", "USD/JPY", "JP10Y", "JP3Y",
                        "SP500", "US3Y", "US10Y")
log_data <- na.omit(log_data)

index_col <- data_set$date[c(-1)]
log_data$JP3Y[is.infinite(log_data$JP3Y)] <- NA
log_data$JP10Y[is.infinite(log_data$JP10Y)] <- NA

zoo_data <- zoo(log_data, order.by = index_col)
zoo_data <- na.omit(zoo_data)
#--------------------------------

result_dd <- ConnectednessApproach(zoo_data, nlag=1, nfore=20, model = "",
                                   window.size = 30)
PlotNET(result_dd)
PlotNetwork(result_dd, method="NPDC")

#--------------------------------

par(mfrow = c(2, 2))
plot(zoo_data$KOSPI, col ="#003366", lwd = 0.5, xlab = NA, ylab = NA, main = "KOSPI",font.main = 1)
plot(zoo_data$KR3Y, col ="#003366", lwd = 0.5, xlab = NA, ylab = NA, main = "KR3Y",font.main = 1)
plot(zoo_data$KR10Y, col ="#003366", lwd = 0.5, xlab = NA, ylab = NA, main = "KR10Y",font.main = 1)
plot(zoo_data$`USD/KRW`, col ="#003366", lwd = 0.5, xlab = NA, ylab = NA, main = "USD/KRW",font.main = 1)

par(mfrow = c(2, 2))
plot(zoo_data$NIKKEI, col ="#003300", lwd = 0.5, xlab = NA, ylab = NA, main = "NIKKEI",font.main = 1)
plot(zoo_data$JP3Y, col ="#003300", lwd = 0.5, xlab = NA, ylab = NA, main = "JP3Y",font.main = 1)
plot(zoo_data$JP10Y, col ="#003300", lwd = 0.5, xlab = NA, ylab = NA, main = "JP10Y",font.main = 1)
plot(zoo_data$`USD/JPY`, col ="#003300", lwd = 0.5, xlab = NA, ylab = NA, main = "USD/JPY",font.main = 1)

par(mfrow = c(3,1))
plot(zoo_data$SP500, col ="#000033", lwd = 0.5, xlab = NA, ylab = NA, main = "SP500",font.main = 1)
plot(zoo_data$US3Y, col ="#000033", lwd = 0.5, xlab = NA, ylab = NA, main = "US3Y",font.main = 1)
plot(zoo_data$US10Y, col ="#000033", lwd = 0.5, xlab = NA, ylab = NA, main = "US10Y",font.main = 1)


par(mfrow = c(2, 2))
plot(total_korea$date, total_korea$kospi, type="l" ,col ="#003366", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "KOSPI",font.main = 1)
plot(total_korea$date, total_korea$'3_bond_ko', type="l" ,col ="#003366", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "KR3Y",font.main = 1, yaxt = "n")
axis(2, at = pretty(total_korea$'3_bond_ko'), labels = paste0(pretty(total_korea$'3_bond_ko') *1 , "%"))
plot(total_korea$date, total_korea$'10_bond_ko', type="l" ,col ="#003366", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "KR10Y",font.main = 1, yaxt = "n")
axis(2, at = pretty(total_korea$'10_bond_ko'), labels = paste0(pretty(total_korea$'10_bond_ko') *1 , "%"))
plot(total_korea$date, total_korea$korea_ex, type="l" ,col ="#003366", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "USD/KRW",font.main = 1)

par(mfrow = c(2, 2))
plot(total_jp$date, total_jp$nikkei, type="l" ,col ="#003300", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "NIKKEI",font.main = 1)
plot(total_jp$date, total_jp$bond_jp_3, type="l" ,col ="#003300", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "JP3Y",font.main = 1, yaxt = "n")
axis(2, at = pretty(total_jp$bond_jp_3), labels = paste0(pretty(total_jp$bond_jp_3) *1 , "%"))
plot(total_jp$date, total_jp$bond_jp_10, type="l" ,col ="#003300", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "JP10Y",font.main = 1, yaxt = "n")
axis(2, at = pretty(total_jp$bond_jp_10), labels = paste0(pretty(total_jp$bond_jp_10) *1 , "%"))
plot(total_jp$date, total_jp$japan_ex, type="l" ,col ="#003300", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "USD/JPY",font.main = 1)

par(mfrow = c(3,1))
plot(total_us$date, total_us$sp500, type="l" ,col ="#000033", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "S&P500",font.main = 1)
plot(total_us$date, total_us$bond_us_3, type="l" ,col ="#000033", lwd = 0.5, xlab = NA, ylab = NA, 
     main = "",font.main = 1)


