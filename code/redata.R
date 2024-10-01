library(quantmod)
library(dplyr)
library(zoo)

# Kospi
kospi_raw <- quantmod::getSymbols("^KS11", from="1990-01-01", to="2024-09-24",
                                  auto.assign = F)
kospi_raw$KS11.Close
kospi <- data.frame(
  date = index(kospi_raw), KOSPI= kospi_raw$KS11.Close
)

# Nikkei
nikkei_raw <- quantmod::getSymbols("^N225", from="1990-01-01", to="2024-09-24",
                                   auto.assign = F)
nikkei <- KS11.Adjustednikkei <- data.frame(
  date = index(nikkei_raw), NIKKEI= nikkei_raw$N225.Close
)

# SP500
sp500_raw <- quantmod::getSymbols("^GSPC", from="1990-01-01", to="2024-09-24",
                                   auto.assign = F)
sp500 <- data.frame(
  date = index(sp500_raw), SP500= sp500_raw$GSPC.Close
)

# processing
merge_set <- Reduce(function(x,y) full_join(x, y), list(kospi, nikkei, sp500))

table(is.na(merge_set))

merge_set <- na.omit(merge_set)

# log diff
data_set <- data.frame(
  KOSPI = diff(log(merge_set$KS11.Close)),
  NIKKEI = diff(log(merge_set$N225.Close)),
  SP500 = diff(log(merge_set$GSPC.Close))
)


# volatility
TRADING_DAYS <- 22

volatility_set <- data.frame(
  KOSPI = runSD(data_set$KOSPI, n = TRADING_DAYS) * sqrt(TRADING_DAYS),
  NIKKEI = runSD(data_set$NIKKEI, n = TRADING_DAYS) * sqrt(TRADING_DAYS),
  SP500 = runSD(data_set$SP500, n = TRADING_DAYS) * sqrt(TRADING_DAYS))
volatility_set <- na.omit(volatility_set)

# zoo data
date_col <- merge_set$date[c(-1)]
zoo_data <- zoo(volatility_set, order.by = date_col)



