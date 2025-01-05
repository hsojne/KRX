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

###------------------------------------ Extract Data ------------------------------------###

## Part 1 : log return setup and GARCH model setup

# load data ---
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

# forward fill
total.stock <- Reduce(function(x,y) full_join(x, y, by ="date"), list(kospi, nikkei, sp500))
total.stock <- total.stock[c(1:7023), ] %>% na.locf(na.rm = FALSE) %>% na.omit

# daily return
zoo_total.stock <- zoo(total.stock[c(-1)], order.by = total.stock$date)
daily.return <- diff(log(zoo_total.stock))

# GARCH(1, 1) model
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

zoo_garch <- zoo(daily.volatility[c(-1)], order.by =daily.volatility$date)



## ------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------ ##


