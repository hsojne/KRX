library(ConnectednessApproach)
library(vars)
library(dplyr)
library(forecast)

# select lag QVAR

lag_selection <- VARselect(vol_data, lag.max = 10, type="none")
lag_selection


### Spillover 
q_99 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                                  model = "QVAR",
                                  VAR_config = list(QVAR = list(tau = c(0.99))),
                                  window.size = 120)

q_95 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.95))),
                              window.size = 120)

q_75 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.75))),
                              window.size = 120)

q_50 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.5))),
                              window.size = 120)

q_25 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.25))),
                              window.size = 120)

q_05 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.05))),
                              window.size = 120)

q_01 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.01))),
                              window.size = 120)
PlotNET(q_99)
PlotNET(q_95)
PlotNET(q_75)
PlotNET(q_50)
PlotNET(q_25)
PlotNET(q_05)
PlotNET(q_01)



