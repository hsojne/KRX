### Code Cleanup ###

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

###------------------------------------ QVAR model ------------------------------------###

# func QVAR
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

# lag = 5, h= 5
lag55.120 <- qvar(lag = 5, horizon = 5, Q = c("0.95", "0.5", "0.05"), window =120, cor.vs.data =zoo_garch$KOSPI)
lag55.247 <- qvar(lag = 5, horizon = 5, Q = c("0.95", "0.5", "0.05"), window =247, cor.vs.data =zoo_garch$KOSPI)

# save func
save.QVAR.result <- function(input_data, lag, window_length, daily.volatility) {
  # - set data index
  date_range <- daily.volatility$date[(window_length - lag):length(daily.volatility$date)]
  
  # - bind high, middle and low 
  high_columns <- cbind(input_data$to.high, input_data$net.high)
  middle_columns <- cbind(input_data$to.middle, input_data$net.middle)
  low_columns <- cbind(input_data$to.low, input_data$net.low)
  
  # - merge to DF
  DF = data.frame(
    Date = date_range,
    From.KOSPI.high = high_columns[, 1], From.NIKKEI.high = high_columns[, 2], From.SP500.high = high_columns[, 3],
    From.KOSPI.middle = middle_columns[, 1], From.NIKKEI.middle = middle_columns[, 2], From.SP500.middle = middle_columns[, 3],
    From.KOSPI.low = low_columns[, 1], From.NIKKEI.low = low_columns[, 2], From.SP500.low = low_columns[, 3],
    Net.KOSPI.high = high_columns[, 4], Net.NIKKEI.high = high_columns[, 5], Net.SP500.high = high_columns[, 6],
    Net.KOSPI.middle = middle_columns[, 4], Net.NIKKEI.middle = middle_columns[, 5], Net.SP500.middle = middle_columns[, 6],
    Net.KOSPI.low = low_columns[, 4], Net.NIKKEI.low = low_columns[, 5], Net.SP500.low = low_columns[, 6],
    Total.High = input_data$total.high, Total.middle = input_data$total.middle, Total.low = input_data$total.low
  )
  
  # - return DF
  return(DF)
}


# setup 1 : lag 5, horizon 5 and window size 120
setup.1 <- save.QVAR.result(lag55.120, 5, 120, daily.volatility)
PCA.setup.1 <- setup.1[, c(1, 2:10)]

# setup 2 : lag 5, horizon 5 and window size 247
setup.2 <- save.QVAR.result(lag55.120, 5, 247, daily.volatility)
PCA.setup.2 <- setup.2[, c(1, 2:10)]

# merge return to setup data
daily.return.df <- data.frame(
  Date = index(daily.return), KOSPI = daily.return$KOSPI, NIKKEI = daily.return$NIKKEI,
  SP500 = daily.return$SP500
)


input.to.pca.1 <- Reduce(function(x,y) left_join(x, y, by ="Date"), list(PCA.setup.1, daily.return.df))
input.to.pca.2 <- Reduce(function(x,y) left_join(x, y, by ="Date"), list(PCA.setup.2, daily.return.df))

write_xlsx(input.to.pca.1, "/Users/82108/Desktop/KRW/input.to.pca.1.xlsx")
write_xlsx(input.to.pca.2, "/Users/82108/Desktop/KRW/input.to.pca.2.xlsx")


## ------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------ ##





