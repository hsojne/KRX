### --------------- KRX --------------------

#install.packages("quantmod")
#install.packages("dplyr")
#install.packages("zoo") 
#install.packages("rugarch")
#install.packages("fBasics")
#install.packages("tseries")
#install.packages("ConnectednessApproach")
#install.packages("zoo")
#install.packages("vars")
#install.packages("moments")



library(quantmod)
library(dplyr)
library(zoo)
library(rugarch)
library(fBasics)
library(stats)
library(tseries)
library(ConnectednessApproach)
library(zoo)
library(vars)
library(moments)


## Part 1 : load data ---------------

# KOSPI DATA
kospi_raw <- quantmod::getSymbols("^KS11", from="1990-01-01", to="2024-11-25",
                                  auto.assign = F)
KOSPI <- data.frame(
  date = index(kospi_raw), KOSPI= kospi_raw$KS11.Close)

# NIKKEI DATA
nikkei_raw <- quantmod::getSymbols("^N225", from="1990-01-01", to="2024-11-25",
                                   auto.assign = F)
NIKKEI <- data.frame(
  date = index(nikkei_raw), NIKKEI= nikkei_raw$N225.Close)

# SP500 DATA
sp500_raw <- quantmod::getSymbols("^GSPC", from="1990-01-01", to="2024-11-25",
                                  auto.assign = F)
SP500 <- data.frame(
  date = index(sp500_raw), SP500= sp500_raw$GSPC.Close)


# Part 2 : log diff ---------------
# log diff
log_diff <- data.frame(
  KOSPI = dailyReturn(kospi_raw, type = "log"),
  NIKKEI = dailyReturn(nikkei_raw, type = "log"),
  SP500 = dailyReturn(sp500_raw, type = "log")
)

## ---------------------------------------------------------------------------

# Part 3 : garch model for extract volatility ---------------
## ordinary volatility -----------------------------
TRADING_DAYS <- 22

volatility_set <- data.frame(
  KOSPI = runSD(log_diff$KOSPI, n = TRADING_DAYS) * sqrt(TRADING_DAYS),
  NIKKEI = runSD(log_diff$NIKKEI, n = TRADING_DAYS) * sqrt(TRADING_DAYS),
  SP500 = runSD(log_diff$SP500, n = TRADING_DAYS) * sqrt(TRADING_DAYS))
volatility_set <- na.omit(volatility_set)



summary_statistics <- function(data) {
  # 기본 통계량
  mean_val <- mean(data, na.rm = TRUE)          # 평균
  sd_val <- sd(data, na.rm = TRUE)              # 표준편차
  max_val <- max(data, na.rm = TRUE)            # 최대값
  min_val <- min(data, na.rm = TRUE)            # 최소값
  
  # ADF 검정
  # 단위근 검정
  ## H0 : 비정상성
  adf_test <- adf.test(data)
  adf_p_value <- adf_test$p.value               # ADF 검정의 p-값
  
  # JB 검정
  # 정규성 검정
  ## H0 : 정규분포 가정
  jb_test <- jarque.test(data)
  jb_p_value <- jb_test$p.value              # JB 검정의 p-값
  
  ## KPSS 검정 수행
  # 정상성 검정
  ## H0 : 정상성
  kpss_test <- kpss.test(data)
  kpss_p_value <- kpss_test$p.value
  
  
  
  # 왜도와 척도
  skewness_val <- skewness(data, na.rm = TRUE)  # 왜도
  kurtosis_val <- kurtosis(data, na.rm = TRUE)  # 척도
  
  
  result <- data.frame(
    Mean = mean_val,
    Std_Dev = sd_val,
    Max = max_val,
    Min = min_val,
    ADF_p_value = adf_p_value,
    KPSS_p_value = kpss_p_value,
    JB_p_value = jb_p_value,
    Skewness = skewness_val,
    Kurtosis = kurtosis_val
  )
  
  return(result)
}


summary_statistics(volatility_set$KOSPI)
summary_statistics(volatility_set$NIKKEI)
summary_statistics(volatility_set$SP500)

zoo_data <- zoo(x = volatility_set, order.by =  col[c(23:length(col))])
#---------------------------------------------------


## GARCH Model(1,1) and SP500 is GARCH(1,2)
create.GARCH <- function(data, p, q){
  
  garch1=garch(data, c(p,q))
  out <- summary(garch1)
  theta <- out$coef
  
  y<-data
  sd(y)
  
  n <-length(y)
  sigma2garch <- sd(y)^2*rep(1,n)
  e <- y 
  
  if (q == 1){ 
    for (i in 2:n) {
      sigma2garch[i] <- theta[1] + theta[2]*e[i-1]^2 + theta[3]*sigma2garch[i-1]}
  } else {
    for (i in 3:n) {
      sigma2garch[i] <- theta[1] + theta[2]*e[i-1]^2 + theta[3]*sigma2garch[i-1] + theta[4]*sigma2garch[i-2]}
  }
  
  return(sigma2garch)
}

GARCH.set <- 0
GARCH.set = data.frame(
  data = col[c(2:length(col))],
  KOSPI = create.GARCH(log_diff$KOSPI, 1, 1),
  NIKKEI = create.GARCH(log_diff$NIKKEI, 1, 1),
  SP500 = create.GARCH(log_diff$SP500, 1, 2)
)

par(mfrow=c(4,3))

# raw data
for(i in 2:4){
  plot(merge_set$date, merge_set[[i]], type="l", xlab = "", ylab = "", main = paste0("raw.", colnames(merge_set[i])) ,
       col = rgb(51/255, 0, 102/255, 0.5), font.main = 2, lwd = 0.5)
  grid(NA, NULL,lty = 2)
  lines(x = merge_set$date, y= merge_set[[i]], type = "l",  col = rgb(51/255, 0, 102/255, 0.5), lwd = 1)
}

# daily return data
for(i in 1:3){
  plot( col[c(2:length(col))], log_diff[[i]], type="l", xlab = "", ylab = "", main = paste0("return.", colnames(log_diff[i])),
        col = rgb(0, 51/255, 102/255, 0.5), font.main = 2, lwd=0.5)
  grid(NA, NULL,lty = 2)
  lines(x = col[c(2:length(col))], y= log_diff[[i]], type = "l",  col = rgb(0, 51/255, 102/255, 0.5), lwd = 1)
}

# volatility data
for(i in 1:3){
  plot( col[c(23:length(col))], volatility_set[[i]], type="l", xlab = "", ylab = "", main = paste0("volatility.", colnames(volatility_set[i])),
        col = rgb(0, 51/255, 102/255, 0.5), font.main = 2, lwd=0.5)
  grid(NA, NULL,lty = 2)
  lines(x = col[c(23:length(col))], y= volatility_set[[i]], type = "l",  col = rgb(0, 51/255, 102/255, 0.5), lwd = 1)
}

# garch data
for(i in 2:4){
  plot(GARCH.set$data, GARCH.set[[i]], type="l", xlab = "", ylab = "", main = paste0("garch.", colnames(GARCH.set[i])),
       col = rgb(153/255, 0, 0, 0.5), font.main = 2, lwd=0.5)
  grid(NA, NULL,lty = 2)
  lines(x = col[c(2:length(col))], y= GARCH.set[[i]], type = "l",  col = rgb(153/255, 0, 0, 0.5), lwd = 1)
}



## --------------------------------------------------------------------------

# Part 4 : garch model for extract volatility ---------------
zoo.set <- zoo(x = GARCH.set[c(-1)], order.by = GARCH.set$data)
zoo.vol <- zoo(x = volatility_set, order.by = col[c(23:length(col))])


# Rolling 120 day -------------------------------------------- 



q_95 <- ConnectednessApproach(zoo.vol, nlag = 4, nfore = 22,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.95))),
                              window.size = 120)

q_50 <- ConnectednessApproach(zoo.vol, nlag = 4, nfore = 22,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.5))),
                              window.size = 120)

q_05 <- ConnectednessApproach(zoo.vol, nlag = 4, nfore = 22,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.05))),
                              window.size = 120)





Q_low_rol_120 <- ConnectednessApproach(zoo.set, nlag = 10, nfore = 25,
                                      model = "QVAR",
                                      VAR_config = list(QVAR = list(tau = c(0.05))),
                                      window.size = 120)

Q_mid_rol_120 <- ConnectednessApproach(zoo.set, nlag = 10, nfore = 10,
                                       model = "QVAR",
                                       VAR_config = list(QVAR = list(tau = c(0.5))),
                                       window.size = 120)

Q_high_rol_120 <- ConnectednessApproach(zoo.set, nlag = 10, nfore = 10,
                                        model = "QVAR",
                                        VAR_config = list(QVAR = list(tau = c(0.95))),
                                        window.size = 120)

# save NET Index
low.set = as.data.frame(q_05$NET)
min.set = as.data.frame(q_50$NET)
high.set = as.data.frame(q_95$NET)



total_tsi<- ConnectednessApproach(zoo.vol, nlag = 4, nfore = 10,
                                  model = "QVAR", window.size = 120)


# --------------------------------------------------------------------------

qvar_make_plot <- function(val=data, main=cha, col=cols){
  plot(date_qvar, val, type="l",  ylim=c(-150, 150), xlab="", ylab="", main=main, lwd=0.7)
  grid(NA, NULL, lty=2)
  
  polygon(c(date_qvar, rev(date_qvar)), c(pmin(val, 0), rep(0, length(val))),
          col = col, border = NA)
  polygon(c(date_qvar, rev(date_qvar)), c(pmax(val, 0), rep(0, length(val))),
          col = col, border = NA)
  
  lines(date_qvar, val, type="l", col=col, lwd=0.1)
  abline(h=0, col="#111", lwd=0.1)
}

date_qvar <- seq(from = as.Date("1990-01-01"), to=as.Date("2024-11-25"),
                 length.out = length(low.set$KOSPI))
par(mfrow = c(3,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

# 0.95
qvar_make_plot(high.set$KOSPI, main = "Q = 0.95 KOSPI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(high.set$NIKKEI, main = "Q = 0.95 NIKKEI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(high.set$SP500, main = "Q = 0.95 SP500", col = rgb(153/255, 0, 0, 0.7))

# 0.5
qvar_make_plot(min.set$KOSPI, main = "Q = 0.5 KOSPI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(min.set$NIKKEI, main = "Q = 0.5 NIKKEI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(min.set$SP500, main = "Q = 0.5 SP500", col = rgb(0, 51/255, 102/255, 0.7))

# 0.05
qvar_make_plot(low.set$KOSPI, main = "Q = 0.05 KOSPI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(low.set$NIKKEI, main = "Q = 0.05 NIKKEI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(low.set$SP500, main = "Q = 0.05 SP500", col = rgb(51/255, 0, 102/255, 0.7))




lag_selection <- VARselect(volatility_set, lag.max = 20, type="none")
lag_selection

# --------------------------------------------------------------------------

# Part 5 : new index ---------------

## 1 : simple weighting to 1/3

test.n = length(min.set$KOSPI)

index.1 = list()

h.matrix = as.matrix(high.set)
m.matrix = as.matrix(min.set)
r.matrix = as.matrix(low.set)

for (i in 1:test.n) {
  index.1[i] = 1/3*(h.matrix[i, 1] * m.matrix[i, 1] * r.matrix[i, 1]) +
    1/3*(h.matrix[i, 2] * m.matrix[i, 2] * r.matrix[i, 2]) +
    1/3*(h.matrix[i, 3] * m.matrix[i, 3] * r.matrix[i, 3])
}

test.date <- seq(from = as.Date("1990-01-01"), to=as.Date("2024-11-25"),
           length.out = test.n)

result.index_1 <- data.frame(date = test.date, target = as.double(index.1))

par(mfrow=c(1,1))
plot(result.index_1$date, result.index_1$target, type="l", xlab="", ylab="", main="New index", lwd=0.7,
     col = rgb(0, 51/255, 102/255, 0.7))
grid(NA, NULL, lty=2)

lines(result.index_1$date, result.index_1$target, type="l", col= rgb(0, 51/255, 102/255, 0.7), lwd=0.1)
abline(h=0, col="#111", lwd=0.1)



plot(total_tsi$TCI, type='l', main = "total spillover index", lwd=0.7,
     col = rgb(0, 51/255, 102/255, 0.7))
grid(NA, NULL, lty=2)



get_quantiles <- function(data, window = 22) {
 qqq.set = mutate(q5 = rollapply(value, width = window, FUN = quantile, probs = 0.05, fill = NA, align = 'right'),
           q50 = rollapply(value, width = window, FUN = quantile, probs = 0.50, fill = NA, align = 'right'),
           q95 = rollapply(value, width = window, FUN = quantile, probs = 0.95, fill = NA, align = 'right'))
  return(qqq.set)
}

qqq.data <- get_quantiles(zoo_data$KOSPI)








# Function to get KOSPI index from Yahoo Finance
get_kospi <- function(start_date = '1990-01-01', end_date = '2024-11-25') {
  kospi <- getSymbols('^KS11', src = 'yahoo', from = start_date, to = end_date, auto.assign = FALSE)
  kospi <- data.frame(Date = index(kospi), coredata(kospi))
  return(kospi)
}

# Function to extract quantiles of 95% rolling window of data
get_quantiles <- function(data, window = 22) {
  data <- data %>%
    mutate(q5 = rollapply(value, width = window, FUN = quantile, probs = 0.05, fill = NA, align = 'right'),
           q50 = rollapply(value, width = window, FUN = quantile, probs = 0.50, fill = NA, align = 'right'),
           q95 = rollapply(value, width = window, FUN = quantile, probs = 0.95, fill = NA, align = 'right'))
  return(data)
}

# Get KOSPI data
kospi <- get_kospi()

# Change to dataframe and add date column
kospi <- kospi %>%
  select(Date, value = kospi$KS11.Close) %>%
  mutate(Date = as.Date(Date))

# Calculate quantiles
kospi_quantiles <- get_quantiles(kospi)