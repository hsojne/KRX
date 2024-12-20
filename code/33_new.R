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
library(writexl)
library(ggplot2)
library(rmgarch)

## load data ---
get.stock <- function(ticker, from, to, list_val){
  load.stock = getSymbols(ticker, from=from, to=to, auto.assign=F)
  raw.stock = data.frame(index(load.stock), value=load.stock[, 4])
  colnames(raw.stock) = list_val
  rownames(raw.stock) = NULL
  
  return(raw.stock)
}

kospi <- get.stock("^KS11", from="1997-01-01", to="2024-11-30", list_val = c("date", "KOSPI"))
nikkei <- get.stock("^N225", from="1997-01-01", to="2024-11-30", list_val = c("date", "NIKKEI"))
sp500 <- get.stock("^GSPC", from="1997-01-01", to="2024-11-30", list_val = c("date", "SP500"))

## forward fill
total.stock <- Reduce(function(x,y) full_join(x, y, by ="date"), list(kospi, nikkei, sp500))
total.stock <- total.stock[c(1:7023), ] %>% na.locf(na.rm = FALSE) %>% na.omit

## daily return
zoo_total.stock <- zoo(total.stock[c(-1)], order.by = total.stock$date)
daily.return <- diff(log(zoo_total.stock))

## GARCH(1, 1) model
get.garch <-function(data, p, q){
  spec = ugarchspec(
    variance.model = list(model='sGARCH', garchOrder=c(p, q)),
    mean.model = list(armaOrder=c(0, 0), include.mean=TRUE),
    distribution.model = "std")
  
  # fit
  garch = ugarchfit(spec=spec, data=data)
  
  # coef
  parameters = garch@fit$robust.matcoef
  theta = garch@fit$coef
  
  # df
  y <- data ; n <- length(y) ; e <- y
  
  sigma2garch <- sd(y)^2 * rep(1, n)
  
  for(i in 2:n){
    sigma2garch[i] <- theta[3] * e[i-1]^2 + theta[4] * sigma2garch[i-1]
  }
  
  result <- data.frame(
    data = as.Date(garch@model$modeldata$index),
    value = sigma2garch
  )
  
  # return
  return(result)
}

daily.volatility <- data.frame(
  date = index(daily.return),
  KOSPI = get.garch(daily.return$KOSPI, 1, 1)[, 2],
  NIKKEI = get.garch(daily.return$NIKKEI, 1, 1 )[, 2],
  SP500 = get.garch(daily.return$SP500, 1, 1)[, 2]
)

## QVAR 

VARselect(test)
zoo_garch <- zoo(daily.volatility[c(-1)], order.by =daily.volatility$date)


qvar <- function(lag, horizon, Q, window, cor.vs.data){
  
  
  ## runs QVAR
  risk.high <- ConnectednessApproach(zoo_garch, nlag=lag, nfore=horizon, model="QVAR",
                                     VAR_config = list(QVAR=list(tau = Q[1])), 
                                     window.size = window)
  
  risk.middle <- ConnectednessApproach(zoo_garch, nlag=lag, nfore=horizon, model="QVAR",
                                       VAR_config = list(QVAR=list(tau = Q[2])), 
                                       window.size = window)
  
  risk.low <- ConnectednessApproach(zoo_garch, nlag=lag, nfore=horizon, model="QVAR",
                                    VAR_config = list(QVAR=list(tau = Q[3])), 
                                    window.size = window)
  
  
  ## calculate W by quantiles
  ### high
  result.risk.high <- t(as.data.frame(risk.high$CT))[, 1]
  w.high <- data.frame()
  
  for(i in 1:(length(result.risk.high)/3)){
    w.high[i, 1] <- result.risk.high[(i-1)*3 + 1]
    w.high[i, 2] <- result.risk.high[(i-1)*3 + 2]
    w.high[i, 3] <- result.risk.high[(i-1)*3 + 3] 
    
    colnames(w.high) <- c("From KOSPI", "From NIKKEI", "From SP500")
  }
  
  ### middle  
  result.risk.middle <- t(as.data.frame(risk.middle$CT))[, 1]
  w.middle <- data.frame()
  
  for(i in 1:(length(result.risk.middle)/3)){
    w.middle[i, 1] <- result.risk.middle[(i-1)*3 + 1]
    w.middle[i, 2] <- result.risk.middle[(i-1)*3 + 2]
    w.middle[i, 3] <- result.risk.middle[(i-1)*3 + 3] 
    
    colnames(w.middle) <- c("From KOSPI", "From NIKKEI", "From SP500")
  }
  
  ### low  
  result.risk.low <- t(as.data.frame(risk.low$CT))[, 1]
  w.low <- data.frame()
  
  for(i in 1:(length(result.risk.low)/3)){
    w.low[i, 1] <- result.risk.low[(i-1)*3 + 1]
    w.low[i, 2] <- result.risk.low[(i-1)*3 + 2]
    w.low[i, 3] <- result.risk.low[(i-1)*3 + 3] 
    
    
    colnames(w.low) <- c("From KOSPI", "From NIKKEI", "From SP500")
  }
  
  
  ## cor test matrix
  levels <- list(w.high, w.middle, w.low)
  create.cor_matrix <- matrix(NA, nrow = 6, ncol = 3, dimnames = list(
    c("Q = High", "p-value", "Q = Middle", "p-value", "Q = Low", "p-value"),
    c("From KOSPI", "From NIKKEI", "From S&P500")
  ))
  windows =  window - lag
  cor.vs.datas <- cor.vs.data[windows:length(cor.vs.data)]
  
  for (i in 1:length(levels)) {
    current_level <- levels[[i]]
    
    for (j in 1:3) {
      create.cor_matrix[(i - 1) * 2 + 1, j] <- cor.test(current_level[[j]], cor.vs.datas)$estimate
      create.cor_matrix[(i - 1) * 2 + 2, j] <- cor.test(current_level[[j]], cor.vs.datas)$p.value
    }
  }
  
  return(list(to.high = w.high, to.middle = w.middle, to.low = w.low, cor.table = create.cor_matrix,
              total.high = risk.high$TCI, total.middle = risk.middle$TCI, total.low = risk.low$TCI,
              net.high = risk.high$NET, net.middle = risk.middle$NET, net.low = risk.low$NET))
  
}






## DCC - GARCH
n <- length(daily.return$KOSPI)
returns <- data.frame(
  Korea = daily.return$KOSPI,
  US = daily.return$SP500,
  Japan = daily.return$NIKKEI
)

# set DCC-GARCH
spec_us <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                      variance.model = list(model = "sGARCH"),
                      distribution.model = "norm")

spec_ja <- ugarchspec(mean.model = list(aramaOrder = c(0, 0)),
                      variance.model = list(model = "sGARCH"),
                      distribution.model = "norm")

dcc_spec <- dccspec(uspec = multispec(replicate(3, spec_us, spec_ja)),  
                    dccOrder = c(1, 1),
                    distribution = "mvnorm")

# fit 
dcc_fit <- dccfit(dcc_spec, data = returns)

# extract
dcc_cor <- rcor(dcc_fit)  # 3D result

# plot
par(mfrow=c(2,1))
korea_us_corr <- dcc_cor[1, 2, ]
plot(daily.volatility$date, korea_us_corr, type = "l", col = rgb(51/255, 0, 102/255, 0.7), lwd=1,
     main = "Dynamic Correlation (Korea & US)", xlab = NA, ylab = "Correlation")

korea_ja_corr <- dcc_cor[1, 3, ]
plot(index(daily.return), korea_ja_corr, type = "l", col = rgb(0, 102/255, 51/255, 0.7), lwd = 1,
     main = "Dynamin Correlation (Korea & Japan)", xlab = NA, ylab = "Correlation")





################# INDEX !!!!!!!!!!!!! #################
                                                    

# lag = 5, h= 5
lag55.120 <- qvar(lag = 5, horizon = 5, Q = c("0.95", "0.5", "0.05"), window =120, cor.vs.data =zoo_garch$KOSPI)
lag55.247 <- qvar(lag = 5, horizon = 5, Q = c("0.95", "0.5", "0.05"), window =247, cor.vs.data =zoo_garch$KOSPI)

data_set.2 <- data.frame(
  Date = daily.volatility$date[242:length(daily.volatility$date)],
  
  From.KOSPI.high = lag55.247$to.high[, 1],
  From.NIKKEI.high = lag55.247$to.high[, 2],
  From.SP500.high = lag55.247$to.high[, 3],
  
  From.KOSPI.middle = lag55.247$to.middle[, 1],
  From.NIKKEI.middle = lag55.247$to.middle[, 2],
  From.SP500.middle = lag55.247$to.middle[, 3],
  
  From.KOSPI.low = lag55.247$to.low[, 1],
  From.NIKKEI.low = lag55.247$to.low[, 2],
  From.SP500.low = lag55.247$to.low[, 3],
  
  Net.KOSPI.high = lag55.247$net.high[, 1],
  Net.NIKKEI.high = lag55.247$net.high[, 2],
  Net.SP500.high = lag55.247$net.high[, 3],
  
  Net.KOSPI.middle = lag55.247$net.middle[, 1],
  Net.NIKKEI.middle = lag55.247$net.middle[, 2],
  Net.SP500.middle = lag55.247$net.middle[, 3],
  
  Net.KOSPI.low = lag55.247$net.low[, 1],
  Net.NIKKEI.low = lag55.247$net.low[, 2],
  Net.SP500.low = lag55.247$net.low[, 3],
  
  Total.High = lag55.247$total.high,
  Total.middle = lag55.247$total.middle,
  Total.low = lag55.247$total.low
)

#sheet_list_1 <- list("lag=5, h=5, r=247" = data_set.2)
#write_xlsx(sheet_list_1, path = "/Users/82108/Desktop/krx_result.xlsx")



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









