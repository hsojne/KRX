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


q_95 <- ConnectednessApproach(zoo_garch, nlag=7, nfore=22, model="QVAR",
                              VAR_config = list(QVAR=list(tau = c(0.95))), 
                              window.size = 120)

q_50 <- ConnectednessApproach(zoo_garch, nlag=7, nfore=22, model="QVAR",
                              VAR_config = list(QVAR=list(tau = c(0.5))), 
                              window.size = 120)

q_05 <- ConnectednessApproach(zoo_garch, nlag=7, nfore=22, model="QVAR",
                              VAR_config = list(QVAR=list(tau = c(0.05))), 
                              window.size = 120)


result.q_95 <- t(as.data.frame(q_95$CT))[, 1]
result.q_50 <- t(as.data.frame(q_50$CT))[, 1]
result.q_05 <- t(as.data.frame(q_05$CT))[, 1]


get.w <- function(data){
  w <- data.frame()
  
  for(i in 1:(length(data)/3)){
    w[i, 1] <- data[(i-1)*3 + 1]
    w[i, 2] <- data[(i-1)*3 + 2]
    w[i, 3] <- data[(i-1)*3 + 3]
  }
  
  colnames(w) <- c("From KOSPI", "From NIKKEI", "From SP500")
  return(w)
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




t.05 <- q_05$TCI
t.50 <- q_50$TCI
t.95 <- q_95$TCI
w.05 <- get.w(result.q_05) * 100
w.50 <- get.w(result.q_50) * 100
w.95 <- get.w(result.q_95) * 100

r_us <- data.frame(
  date = daily.volatility$date, corr_us= korea_us_corr)
r_us <- r_us[r_us$date >= "1997-06-12", 2]

r_ja <- data.frame(
  date = daily.volatility$date, corr_ja = korea_ja_corr)
r_ja <- r_ja[r_ja$date >= "1997-06-12", 2]





'''
################# INDEX !!!!!!!!!!!!! #################
                                                    '''


get.result.and.plot <- function(date, data){
  
  index.new.df <- data.frame(
    date = date.index, new.index = data, vol = daily.volatility[daily.volatility$date  >= "1997-06-12", 2],
    ret = kot[kot$date >= "1997-06-12", 2], stock = sto[sto$date >= "1997-06-12", 2]
  )
  zoo_new <- zoo(index.new.df[c(-1)], order.by =index.new.df$date)
  
  # print corr
  cat("################")
  cat('\n')
  
  cat("Corr volatility : ", cor(index.new.df$new.index, index.new.df$vol), '\n')
  cat("Corr return : ", cor(index.new.df$new.index, index.new.df$ret), '\n')
  cat("Corr stock : ", cor(index.new.df$new.index, index.new.df$stock))
  cat("Corr vol with return : ", cor(index.new.df$vol, index.new.df$ret))
  
  cat('\n')
  cat("################")
  
  # rolling window
  corr.vol <- rollapply(zoo_new, width=120, function(x) cor(x[, 1],x[, 2]), by.column=FALSE)
  corr.return <- rollapply(zoo_new, width=120, function(x) cor(x[, 1],x[, 3]), by.column=FALSE)
  corr.stock <- rollapply(zoo_new, width=120, function(x) cor(x[, 1],x[, 4]), by.column=FALSE)
  
  # plot
  par(mfcol=c(3,3))
  
  ##  --- volatility ---
  plot(index.new.df$date, index.new.df$new.index, type="l", col= rgb(0, 51/255, 102/255, 0.7),
       main="new index", xlab="")
  plot(index.new.df$date, index.new.df$vol, type="l", col = rgb(51/255, 0, 102/255, 0.7),
       main="volatility", xlab="")
  plot(index(corr.vol), corr.vol, type="l", col= rgb(102/255, 51/255, 102/255, 0.7),
       main="volatility correlation", xlab="") 
  abline(h=0, col="black", lwd=0.1)
  
  ## --- return ---
  plot(index.new.df$date, index.new.df$new.index, type="l", col= rgb(0, 51/255, 102/255, 0.7),
       main="new index", xlab="")
  plot(index.new.df$date, index.new.df$ret, type="l", col = rgb(0, 51/255, 51/255, 0.7),
       main="retrun", xlab="")
  plot(index(corr.return), corr.return, type="l", col= rgb(102/255, 51/255, 102/255, 0.7),
       main="return correlation", xlab="") 
  abline(h=0, col="black", lwd=0.1)
  
  ## --- Stock ---
  plot(index.new.df$date, index.new.df$new.index, type="l", col= rgb(0, 51/255, 102/255, 0.7),
       main="new index", xlab="")
  plot(index.new.df$date, index.new.df$stock, type="l", col = rgb(51/255, 51/255, 51/255, 0.7),
       main="stock", xlab="" )
  plot(index(corr.stock), corr.stock, type="l", col= rgb(102/255, 51/255, 102/255, 0.7),
       main="stock correlation", xlab="") 
  abline(h=0, col="black", lwd=0.1)
  
}

date.index = daily.volatility[daily.volatility$date  >= "1997-06-12", 1]

# 1 --- PI [corr * w]
index.new.1 <- double()

for(i in 1:length(date.index)){
  
  index.new.1[i] <- t.95[i]*w.95[i, 1] * t.50[i]*w.50[i, 1] * t.05[i]*w.05[i, 1] +
    r_ja[i]*t.95[i]*w.95[i, 2] * r_ja[i]*t.50[i]*w.50[i, 2] * r_ja[i]*t.05[i]*w.05[i, 2] +
    r_us[i]*t.95[i]*w.95[i, 3] * r_us[i]*t.50[i]*w.50[i, 3] * r_us[i]*t.05[i]*w.05[i, 3]
}


get.result.and.plot(date.index, index.new.1)


# 2 --- SUM [corr * w]
index.new.2 <- double()
for(i in 1:length(date.index)){
  
  index.new.2[i] <- t.95[i]*w.95[i, 1] + t.50[i]*w.50[i, 1] + t.05[i]*w.05[i, 1] +
    r_ja[i]*t.95[i]*w.95[i, 2] + r_ja[i]*t.50[i]*w.50[i, 2] + r_ja[i]*t.05[i]*w.05[i, 2] +
    r_us[i]*t.95[i]*w.95[i, 3] + r_us[i]*t.50[i]*w.50[i, 3] + r_us[i]*t.05[i]*w.05[i, 3]
}

get.result.and.plot(date.index, index.new.2)

# 3 --- SUM [w]
index.new.3 <- double()

for(i in 1:length(date.index)){
  
  index.new.3[i] <- t.95[i]*w.95[i, 1] + t.50[i]*w.50[i, 1] + t.05[i]*w.05[i, 1] +
    t.95[i]*w.95[i, 2] + t.50[i]*w.50[i, 2] + t.05[i]*w.05[i, 2] +
    t.95[i]*w.95[i, 3] + t.50[i]*w.50[i, 3] + t.05[i]*w.05[i, 3]
}

get.result.and.plot(date.index, index.new.3)

# 4 --- PI [w]
index.new.4 <- double()

for(i in 1:length(date.index)){
  
  index.new.4[i] <- t.95[i]*w.95[i, 1] * t.50[i]*w.50[i, 1] * t.05[i]*w.05[i, 1] +
    t.95[i]*w.95[i, 2] * t.50[i]*w.50[i, 2] * t.05[i]*w.05[i, 2] +
    t.95[i]*w.95[i, 3] * t.50[i]*w.50[i, 3] * t.05[i]*w.05[i, 3]
}

get.result.and.plot(date.index, index.new.4)





















