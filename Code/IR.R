qvar_make_plot <- function(val=data, main=cha, col=cols){
  plot(date_qvar, val, type="l", xlab="", ylab="", main=main, lwd=0.7)
  grid(NA, NULL, lty=2)
  
  polygon(c(date_qvar, rev(date_qvar)), c(pmin(val, 0), rep(0, length(val))),
          col = col, border = NA)
  polygon(c(date_qvar, rev(date_qvar)), c(pmax(val, 0), rep(0, length(val))),
          col = col, border = NA)
  
  lines(date_qvar, val, type="l", col=col, lwd=0.1)
  abline(h=0, col="#111", lwd=0.1)
}

date_qvar <- data_set.2$Date
par(mfrow = c(3,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))


# 0.95
qvar_make_plot(data_set.2$From.KOSPI.High, main = "Q = 0.95 KOSPI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(data_set.2$From.NIKKEI.High, main = "Q = 0.95 NIKKEI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(data_set.2$From.SP500.High, main = "Q = 0.95 SP500", col = rgb(153/255, 0, 0, 0.7))

# 0.5
qvar_make_plot(data_set.2$From.KOSPI.Middle, main = "Q = 0.5 KOSPI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(data_set.2$From.NIKKEI.Middle, main = "Q = 0.5 NIKKEI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(data_set.2$From.SP500.Middle, main = "Q = 0.5 SP500", col = rgb(0, 51/255, 102/255, 0.7))

# 0.05
qvar_make_plot(data_set.2$From.KOSPI.low, main = "Q = 0.05 KOSPI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(data_set.2$From.NIKKEI.low, main = "Q = 0.05 NIKKEI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(data_set.2$From.SP500.low, main = "Q = 0.05 SP500", col = rgb(51/255, 0, 102/255, 0.7))


par(mfrow = c(3,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))


qvar_make_plot(data_set.2$Net.KOSPI.High, main = "Q = 0.95 KOSPI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(data_set.2$Net.NIKKEI.High, main = "Q = 0.95 NIKKEI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(data_set.2$Net.SP500.High, main = "Q = 0.95 SP500", col = rgb(153/255, 0, 0, 0.7))

# 0.5
qvar_make_plot(data_set.2$Net.KOSPI.middel, main = "Q = 0.5 KOSPI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(data_set.2$Net.NIKKEI.middel, main = "Q = 0.5 NIKKEI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(data_set.2$Net.SP500.middel, main = "Q = 0.5 SP500", col = rgb(0, 51/255, 102/255, 0.7))

# 0.05
qvar_make_plot(data_set.2$Net.KOSPI.low, main = "Q = 0.05 KOSPI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(data_set.2$Net.NIKKEI.low, main = "Q = 0.05 NIKKEI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(data_set.2$Net.SP500.low, main = "Q = 0.05 SP500", col = rgb(51/255, 0, 102/255, 0.7))



par(mfrow = c(3, 1), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))


qvar_make_plot(data_set.2$TCI, main = "Q = 0.95", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(data_set.2$TCI.1, main = "Q = 0.95", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(data_set.2$TCI.2, main = "Q = 0.95", col = rgb(51/255, 0, 102/255, 0.7))



qvar_model_high <- QVAR(zoo_garch, configuration = list(tau = 0.70, nlag=5))
irf_results_high <- IRF(Phi = qvar_model_high$B, Sigma = qvar_model_high$Q, nfore = 100)
plot_irf_grid(irf_results_high)

qvar_model_middle <- QVAR(zoo_garch, configuration = list(tau = 0.5, nlag=5))
irf_results_middle <- IRF(Phi = qvar_model_middle$B, Sigma = qvar_model_middle$Q, nfore = 30)

qvar_model_low <- QVAR(zoo_garch, configuration = list(tau = 0.05, nlag=5))
irf_results_low <- IRF(Phi = qvar_model_low$B, Sigma = qvar_model_low$Q, nfore = 30)





plot_irf_grid <- function(irf_results, rows = 3, cols = 3) {
  
  par(mfrow = c(3,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
      mgp = c(1, 0.4, 0))
  
  for (i in 1:length(irf_results$irf)) {  
    for (j in 1:ncol(irf_results$irf[[i]])) {  
      plot(
        irf_results$irf[[i]][, j],
        type = "l",
        main = paste("Shock", i, "-> Response", j),
        xlab = "Time",
        ylab = "Response", 
        lwd = 1.5
      )
      grid(NA, NULL, lty=2)
    }
  }
  plot.stepfun
}

plot_irf_grid(irf_results_high)
plot_irf_grid(irf_results_middle)
plot_irf_grid(irf_results_low)



