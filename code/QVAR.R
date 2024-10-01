library(ConnectednessApproach)
library(zoo)

## -- Modeling

# create zoo data
val_set_part <- val_set[c(-1)]
zoo_data <- zoo(x = val_set_part, order.by = val_set$date_col)

# QVAR
Q_low <- ConnectednessApproach(zoo_data, nlag = 1, nfore = 10,
                                model = "QVAR",
                                VAR_config = list(QVAR = list(tau = c(0.05))))

Q_mid <- ConnectednessApproach(zoo_data, nlag = 1, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.5))))

Q_high <- ConnectednessApproach(zoo_data, nlag = 1, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.95))))

# Rolling 30 day ------------------------------------------------------------------------
Q_low_rol<- ConnectednessApproach(zoo_data, nlag = 1, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.05))),
                               window.size = 30)

Q_mid_rol <- ConnectednessApproach(zoo_data, nlag = 1, nfore = 10,
                               model = "QVAR",
                               VAR_config = list(QVAR = list(tau = c(0.5))),
                               window.size = 30)

Q_high_rol <- ConnectednessApproach(zoo_data, nlag = 1, nfore = 10,
                                model = "QVAR",
                                VAR_config = list(QVAR = list(tau = c(0.95))),
                                window.size = 30)

# Rolling 120 day ------------------------------------------------------------------------
Q_low_rol_120<- ConnectednessApproach(zoo_data, nlag = 1, nfore = 10,
                                  model = "QVAR",
                                  VAR_config = list(QVAR = list(tau = c(0.05))),
                                  window.size = 120)

Q_mid_rol_120 <- ConnectednessApproach(zoo_data, nlag = 1, nfore = 10,
                                   model = "QVAR",
                                   VAR_config = list(QVAR = list(tau = c(0.5))),
                                   window.size = 120)

Q_high_rol_120 <- ConnectednessApproach(zoo_data, nlag = 1, nfore = 10,
                                    model = "QVAR",
                                    VAR_config = list(QVAR = list(tau = c(0.95))),
                                    window.size = 120)

# plot net ------------------------------------------------------------------------
PlotNET(Q_low_rol)
PlotNET(Q_low_rol_120)

PlotNET(Q_mid_rol)
PlotNET(Q_mid_rol_120)

PlotNET(Q_high_rol)
PlotNET(Q_high_rol_120)

PlotTCI(Q_low_rol_120)
PlotTCI(Q_mid_rol_120)
PlotTCI(Q_high_rol_120)

