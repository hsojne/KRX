library(stargazer)
library(ggplot2)

a <- VAR_model$TABLE
b <- QVAR_model$TABLE
png(filename = "Desktop/KRX/qvar.png", width = 2000, height = 700, res = 150)
textplot(capture.output(stargazer(b, type="text")),
         halign = "center", valign = "top", cex = 0.8)
dev.off()



var_test <- VAR_model_rolling_120$NET
qvar_test <- QVAR_model_rolling_120$NET
x <- seq(from = as.Date("2010-07-20"), to = as.Date("2024-09-10"), 
         length.out = length(var_test[, "KOSPI_diff"]))


col_make <- function(val = data, main = cha, col_val = cols) {
  plot(x, val, type = "l", ylim = c(-50, 80), xlab = "", ylab = "", main = main)
  grid(NA, NULL,lty = 2)

  polygon(c(x, rev(x)), c(pmin(val, 0), rep(0, length(val))), 
          col = col_val, border = NA)
  polygon(c(x, rev(x)), c(pmax(val, 0), rep(0, length(val))), 
          col = col_val, border = NA)
  
  # 선 그리기
  lines(x, val, type = "l", col = "black", lwd = 1)
  
  # 기준선 그리기
  abline(h = 0, col = "black", lwd = 1)
  }




par(mfcol = c(4,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

# ----------------------------------------------------------------
png("/Users/shiki/Desktop/var_test.png", width = 2000, height = 1500, res = 300)

par(mfcol = c(4,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))
col_make(var_test[, "KOSPI_diff"], main = "KOSPI", col_val = rgb(153/255, 0, 0, 0.5))
col_make(var_test[, "USD_KRW_diff"], main = "USD/KRW", col_val = rgb(153/255, 0, 0, 0.5))
col_make(var_test[, "KR3Y_diff"], main = "KR3Y", col_val = rgb(153/255, 0, 0, 0.5))
col_make(var_test[, "KR10Y_diff"], main = "KR10Y", col_val = rgb(153/255, 0, 0, 0.5))

col_make(var_test[, "NIKKEI_diff"], main = "NIKKEI", col_val = rgb(0, 0, 153/255, 0.5))
col_make(var_test[, "USD_JPY_diff"], main = "USD/JPY", col_val = rgb(0, 0, 153/255, 0.5))
col_make(var_test[, "JP3Y_diff"], main = "JP3Y", col_val = rgb(0, 0, 153/255, 0.5))
col_make(var_test[, "JP10Y_diff"], main = "JP10Y", col_val = rgb(0, 0, 153/255, 0.5))

col_make(var_test[, "SP500_diff"], main = "SP500", col_val = rgb(0, 153/255, 0, 0.5))
col_make(var_test[, "US3Y_diff"], main = "US3Y", col_val = rgb(0, 153/255, 0, 0.5))
col_make(var_test[, "US10Y_diff"], main = "US10Y", col_val = rgb(0, 153/255, 0, 0.5))

dev.off()

# ----------------------------------------------------------------
png("/Users/shiki/Desktop/qvar_test.png", width = 2000, height = 1500, res = 300)

par(mfcol = c(4,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

col_make(qvar_test[, "KOSPI_diff"], main = "KOSPI", col_val = rgb(153/255, 0, 0, 0.5))
col_make(qvar_test[, "USD_KRW_diff"], main = "USD/KRW", col_val = rgb(153/255, 0, 0, 0.5))
col_make(qvar_test[, "KR3Y_diff"], main = "KR3Y", col_val = rgb(153/255, 0, 0, 0.5))
col_make(qvar_test[, "KR10Y_diff"], main = "KR10Y", col_val = rgb(153/255, 0, 0, 0.5))

col_make(qvar_test[, "NIKKEI_diff"], main = "NIKKEI", col_val = rgb(0, 0, 153/255, 0.5))
col_make(qvar_test[, "USD_JPY_diff"], main = "USD/JPY", col_val = rgb(0, 0, 153/255, 0.5))
col_make(qvar_test[, "JP3Y_diff"], main = "JP3Y", col_val = rgb(0, 0, 153/255, 0.5))
col_make(qvar_test[, "JP10Y_diff"], main = "JP10Y", col_val = rgb(0, 0, 153/255, 0.5))

col_make(qvar_test[, "SP500_diff"], main = "SP500", col_val = rgb(0, 153/255, 0, 0.5))
col_make(qvar_test[, "US3Y_diff"], main = "US3Y", col_val = rgb(0, 153/255, 0, 0.5))
col_make(qvar_test[, "US10Y_diff"], main = "US10Y", col_val = rgb(0, 153/255, 0, 0.5))

dev.off()


# -------------------------------------------------

png("/Users/shiki/Desktop/tic_tt.png", width = 2000, height = 1500, res = 300)
par(mfcol = c(1, 1), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))
plot(x, VAR_model_rolling_120$TCI, type="l", xlim=c(x[1], x[3177]),ylim=c(40, 70),
     xlab = "", ylab = "", main = "TSI Rolling 120", col ="#003366")
grid(NA, NULL,lty = 2)
lines(x, QVAR_model_rolling_120$TCI, type = "l", col ="#003300", lwd = 1)
lines(x, VAR_model_rolling_120$TCI, type = "l", col ="#003366", lwd = 1)
legend( 1, 1, legend=c("VAR","QVAR"),fill=c("#003366","#003300"),
       border=NA ,box.lty=0, cex=1)
dev.off()

# -------------------------------------------------


png("/Users/shiki/Desktop/tic.png", width = 2000, height = 1500, res = 300)
par(mfcol = c(2, 1), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))
plot(x[c(-1)], diff(VAR_model_rolling_120$TCI), type="l", ylim=c(-7, 10), 
     xlab = "", ylab = "", main = "VAR TSI")
grid(NA, NULL,lty = 2)
lines(x[c(-1)], diff(VAR_model_rolling_120$TCI), type = "l", col = "black", lwd = 1)

plot(x[c(-1)], diff(QVAR_model_rolling_120$TCI), type="l", ylim=c(-7, 10),
     xlab = "", ylab = "",main = "QVAR TSI")
grid(NA, NULL,lty = 2)
lines(x[c(-1)], diff(QVAR_model_rolling_120$TCI), type = "l", col = "black", lwd = 1)
dev.off()

