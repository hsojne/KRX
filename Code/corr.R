library(forecast)
library(lmtest)


normalized_data <- apply(raw_data[c(-1)], 2, function(x) (x / x[1]) * 100)

png("/Users/82108/Desktop/raw_data.png", width = 2000, height = 1500, res = 300)
par(mfrow = c(1,1), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

par(new=TRUE)
plot(raw_data$date, 
     normalized_data[,1], 
     type = "l", col = rgb(51/255, 0, 102/255, 0.7), lwd = 2, 
     ylim = c(min(normalized_data), max(normalized_data)),
     ylab = "Normalized Index (Base = 100)", 
     xlab = "Date", 
     main = "KOSPI, Nikkei, S&P 500 Comparison")
grid(NA, NULL, lty=2)

lines(raw_data$date, normalized_data[,2], col = rgb(0, 51/255, 102/255, 0.7), lwd = 2)
lines(raw_data$date, normalized_data[,3], col = rgb(153/255, 0, 0, 0.7), lwd = 2)

legend("topleft", 
       legend = c("KOSPI", "Nikkei", "S&P 500"), 
       col = c(rgb(51/255, 0, 102/255, 0.7),  rgb(0, 51/255, 102/255, 0.7), rgb(153/255, 0, 0, 0.7)), 
       lty = 1, 
       lwd = 2)

dev.off()



## TSI

tsi_date <- seq(from = as.Date("1997-6-18"), to=as.Date("2024-09-20"),
                 length.out = length(tsi))



plot(tsi_date, tsi, 
     type = "l", col = rgb(28/255, 36/255, 58/255, 0.6),  lwd=0.1, 
     ylim = c(min(tsi), max(tsi)),
     ylab = "", 
     xlab = "", 
     main = "TSI")

grid(NA, NULL, lty=2)

polygon(c(tsi_date, rev(tsi_date)), c(pmin(tsi, 0), rep(0, length(tsi))),
        col = rgb(28/255, 36/255, 58/255, 0.6), border = NA)
polygon(c(tsi_date, rev(tsi_date)), c(pmax(tsi, 0), rep(0, length(tsi))),
        col = rgb(28/255, 36/255, 58/255, 0.6), border = NA)

lines(tsi_date, tsi, type="l", col=rgb(28/255, 36/255, 58/255, 0.6), lwd=0.1)
abline(h=0, col="black", lwd=0.1)

par(new=TRUE)

plot(raw_data$date, 
     normalized_data[,1], 
     type = "l", col = rgb(51/255, 0, 102/255, 1), lwd = 2, 
     ylim = c(min(normalized_data), max(normalized_data)),
     ylab = "", 
     xlab = "", 
     main = "TSI",
     xaxt='n',
     yaxt='n')

lines(raw_data$date, normalized_data[,2], col = rgb(0, 51/255, 102/255, 1), lwd = 2)
lines(raw_data$date, normalized_data[,3], col = rgb(153/255, 0, 0, 1), lwd = 2)

legend("topleft", 
       legend = c("KOSPI", "Nikkei", "S&P 500", "TSI"), 
       col = c(rgb(51/255, 0, 102/255, 0.7),  rgb(0, 51/255, 102/255, 0.7), 
               rgb(153/255, 0, 0, 0.7), rgb(28/255, 36/255, 58/255, 0.6)), 
       lty = 1, 
       lwd = 2)












