library(forecast)
library(lmtest)
library(MSBVAR)

# corr
raw_before <- raw_data[raw_data$date < "2020-01-01",  ]
raw_after <- raw_data[raw_data$date >="2020-01-01",  ]


cor.test(raw_before$KOSPI, raw_before$NIKKEI)
cor.test(raw_after$KOSPI, raw_after$NIKKEI)

cor.test(raw_before$KOSPI, raw_before$`S&P500`)
cor.test(raw_after$KOSPI, raw_after$`S&P500`)

# var
zoo_raw <- zoo(raw_data[c(-1)], order.by = raw_data$date)
ConnectednessApproach::ConnectednessApproach(zoo_raw)
ConnectednessApproach::R2Correlations(zoo_raw)


diff1_ko  <- diff(raw_data$KOSPI, 1)
diff1_ja <- diff(raw_data$NIKKEI, 1)

grangertest(diff1_ko ~ diff1_ja, order = 3 )
grangertest(diff1_ja ~ diff1_ko, order = 3 )

grangertest()
