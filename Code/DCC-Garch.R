###------------------------------------ DCC-GARCH model ------------------------------------###

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
