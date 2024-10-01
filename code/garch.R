library(rugarch)
library(quantmod)
library(TTR)
library(zoo)

n <- length(log_data$`Log KOSPI`)

ko_gar <- garch(log_data$`Log KOSPI`, c(3,1))
ko_sigma2 <- sd(log_data$`Log KOSPI`)^2 * rep(1,n)
ko_sig <- summary(ko_gar)
ko_e <- log_data$`Log KOSPI`

jp_gar <- garch(log_data$`Log NIKKEI`, c(1,1))
jp_sigma2 <- sd(log_data$`Log NIKKEI`)^2 * rep(1,n)
jp_sig <- summary(jp_gar)
jp_e <- log_data$`Log NIKKEI`

us_gar <- garch(log_data$`Log S&P500`, c(1,1))
us_sigma2 <- sd(log_data$`Log S&P500`)^2 * rep(1,n)
us_sig <- summary(us_gar)
us_e <- log_data$`Log S&P500`




for (i in 2:n) {
  ko_sigma2[i] <- ko_sig$coef[1,1] + ko_sig$coef[2,1]*ko_e[i-1]^2 + ko_sig$coef[3,1]*ko_sigma2[i-1]
}
for (i in 2:n) {
  jp_sigma2[i] <- jp_sig$coef[1,1] + jp_sig$coef[2,1]*jp_e[i-1]^2 +jp_sig$coef[3,1]*jp_sigma2[i-1]
}
for (i in 2:n) {
  us_sigma2[i] <- us_sig$coef[1,1] + us_sig$coef[2,1]*us_e[i-1]^2 + us_sig$coef[3,1]*us_sigma2[i-1]
}

garch_data <- data.frame(
  "Volatility KOSPI" = ko_sigma2,
  "Volatility NIKKEI" = jp_sigma2,
  "Volatility S&P500" = us_sigma2
)


#

quantile_volatility_95 <- rollapply(garch_data$Volatility.KOSPI, width = 22, 
                                 FUN = function(x) quantile(x, probs = 0.95, na.rm = TRUE), 
                                 by.column = FALSE, align = "right")

quantile_volatility_50 <- rollapply(garch_data$Volatility.KOSPI, width = 22, 
                                 FUN = function(x) quantile(x, probs = 0.5, na.rm = TRUE), 
                                 by.column = FALSE, align = "right")


## 
png("/Users/82108/Desktop/RE_data.png", width = 2000, height = 1500, res = 300)
par(mfrow = c(2,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

for(i in 1:3){
  plot(merge_set$date[c(-1)], log_data[[i]], type="l", xlab = "", ylab = "", main = colnames(log_data[i]),
       col = rgb(0, 51/255, 102/255, 0.5), font.main = 2, lwd=0.5)
  grid(NA, NULL,lty = 2)
  lines(x = merge_set$date[c(-1)], y= log_data[[i]], type = "l",  col = rgb(0, 51/255, 102/255, 0.5), lwd = 1)
}


for(i in 1:3){
  plot(raw_data$date[c(-1)], garch_data[[i]], type="l", xlab = "", ylab = "", main = colnames(garch_data[i]),
       col = rgb(153/255, 0, 0, 0.5), font.main = 2, lwd=0.5)
  grid(NA, NULL,lty = 2)
  lines(x = raw_data$date[c(-1)], y= garch_data[[i]], type = "l",  col = rgb(153/255, 0, 0, 0.5), lwd = 1)
}

dev.off()