library(tseries)
library(moments)

# previous 
raw_data <- merge_set
colnames(raw_data) <- c("date", "KOSPI", "NIKKEI", "S&P500")

log_data <- data_set
colnames(log_data)<- c("Log KOSPI", "Log NIKKEI", "Log S&P500")

vol_data <- volatility_set
colnames(vol_data) <- c("Volatility KOSPI", "Volatility NIKKEI", "Volatility S&P500")

# plot
png("/Users/82108/Desktop/RE_data.png", width = 2000, height = 1500, res = 300)
par(mfcol = c(3,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

for(i in 2:4){
  plot(merge_set$date, raw_data[[i]], type="l", xlab = "", ylab = "", main = colnames(raw_data[i]),
       col = rgb(51/255, 0, 102/255, 0.5), font.main = 2, lwd = 0.5)
  grid(NA, NULL,lty = 2)
  lines(x = merge_set$date, y= raw_data[[i]], type = "l",  col = rgb(51/255, 0, 102/255, 0.5), lwd = 1)
  }
  
for(i in 1:3){
  plot(merge_set$date[c(-1)], log_data[[i]], type="l", xlab = "", ylab = "", main = colnames(log_data[i]),
       col = rgb(0, 51/255, 102/255, 0.5), font.main = 2, lwd=0.5)
  grid(NA, NULL,lty = 2)
  lines(x = merge_set$date[c(-1)], y= log_data[[i]], type = "l",  col = rgb(0, 51/255, 102/255, 0.5), lwd = 1)
}

for(i in 1:3){
  plot(col[c(22:length(col))], vol_data[[i]], type="l", xlab = "", ylab = "", main = colnames(vol_data[i]),
       col = rgb(153/255, 0, 0, 0.5), font.main = 2, lwd=0.5)
  grid(NA, NULL,lty = 2)
  lines(x = col[c(22:length(col))], y= vol_data[[i]], type = "l",  col = rgb(153/255, 0, 0, 0.5), lwd = 1)
}

dev.off()

# test

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


summary_statistics(vol_data$`Volatility KOSPI`)
summary_statistics(vol_data$`Volatility NIKKEI`)
summary_statistics(vol_data$`Volatility S&P500`)

# QVAR plot

date_qvar <- seq(from = as.Date("1997-6-18"), to=as.Date("2024-09-20"),
                 length.out = length(q_99$NET[, "KOSPI"]))
qvar_make_plot <- function(val=data, main=cha, col=cols){
  plot(date_qvar, val, type="l",  ylim=c(-150, 150), xlab="", ylab="", main=main, lwd=0.7)
  grid(NA, NULL, lty=2)
  
  polygon(c(date_qvar, rev(date_qvar)), c(pmin(val, 0), rep(0, length(val))),
          col = col, border = NA)
  polygon(c(date_qvar, rev(date_qvar)), c(pmax(val, 0), rep(0, length(val))),
          col = col, border = NA)
  
  lines(date_qvar, val, type="l", col="black", lwd=0.1)
  abline(h=0, col="#111", lwd=0.1)
}


png("/Users/82108/Desktop/q99_50.png", width = 2000, height = 1500, res = 300)
par(mfrow = c(4,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

# 0.99
qvar_make_plot(q_99$NET[, "KOSPI"], main = "Q = 0.99 KOSPI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(q_99$NET[, "NIKKEI"], main = "Q = 0.99 NIKKEI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(q_99$NET[, "SP500"], main = "Q = 0.99 SP500", col = rgb(51/255, 0, 102/255, 0.7))

# 0.95
qvar_make_plot(q_95$NET[, "KOSPI"], main = "Q = 0.95 KOSPI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(q_95$NET[, "NIKKEI"], main = "Q = 0.95 NIKKEI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(q_95$NET[, "SP500"], main = "Q = 0.95 SP500", col = rgb(0, 51/255, 102/255, 0.7))

# 0.75
qvar_make_plot(q_75$NET[, "KOSPI"], main = "Q = 0.75 KOSPI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(q_75$NET[, "NIKKEI"], main = "Q = 0.75 NIKKEI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(q_75$NET[, "SP500"], main = "Q = 0.75 SP500", col = rgb(153/255, 0, 0, 0.7))

# 0.5
qvar_make_plot(q_50$NET[, "KOSPI"], main = "Q = 0.50 KOSPI", col = rgb(0, 102/255, 102/255, 0.7))
qvar_make_plot(q_50$NET[, "NIKKEI"], main = "Q = 0.50 NIKKEI", col = rgb(0, 102/255, 102/255, 0.7))
qvar_make_plot(q_50$NET[, "SP500"], main = "Q = 0.50 SP500", col = rgb(0, 102/255, 102/255, 0.7))

dev.off()


png("/Users/82108/Desktop/q25_01.png", width = 2000, height = 1500, res = 300)
par(mfrow = c(3,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

# 0.25
qvar_make_plot(q_25$NET[, "KOSPI"], main = "Q = 0.25 KOSPI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(q_25$NET[, "NIKKEI"], main = "Q = 0.25 NIKKEI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(q_25$NET[, "SP500"], main = "Q = 0.25 SP500", col = rgb(51/255, 0, 102/255, 0.7))

# 0.05
qvar_make_plot(q_05$NET[, "KOSPI"], main = "Q = 0.05 KOSPI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(q_05$NET[, "NIKKEI"], main = "Q = 0.05 NIKKEI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(q_05$NET[, "SP500"], main = "Q = 0.05 SP500", col = rgb(0, 51/255, 102/255, 0.7))

# 0.01
qvar_make_plot(q_01$NET[, "KOSPI"], main = "Q = 0.01 KOSPI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(q_01$NET[, "NIKKEI"], main = "Q = 0.01 NIKKEI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(q_01$NET[, "SP500"], main = "Q = 0.01 SP500", col = rgb(153/255, 0, 0, 0.7))

dev.off()

par(mfrow = c(3,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

PlotNetwork(q_01, threshold = 0.1)
PlotNetwork(q_05, threshold = 0.1)
 PlotNetwork(q_25, threshold = 0.1)
PlotNetwork(q_50, threshold = 0.1)
PlotNetwork(q_75, threshold = 0.1)
PlotNetwork(q_95, threshold = 0.1)
PlotNetwork(q_99, threshold = 0.1)
