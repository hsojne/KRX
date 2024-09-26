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
###################



total_tsi<- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                              model = "QVAR", window.size = 120)
tsi <- total_tsi$TCI


###################

qq_99 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                              model = "QVAR",
                              VAR_config = list(QVAR = list(tau = c(0.99))))

qq_95 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.95))))

qq_75 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.75))))

qq_50 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.50))))

qq_25 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.25))))

qq_5 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.05))))
qq_1 <- ConnectednessApproach(zoo_data, nlag = 3, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.01))))