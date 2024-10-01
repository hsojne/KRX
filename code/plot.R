## plot part


# plot the raw data
png("/Users/82108/Desktop/raw_data.png", width = 2000, height = 1500, res = 300)
par(mfcol = c(4,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

for(i in 2:5){
  plot(x = data$date, y= data[[i]], type="l", xlab = "", ylab = "", main = colnames(val_set[i]),
       col = rgb(153/255, 0, 0, 0.5), font.main = 2)
  grid(NA, NULL,lty = 2)
  lines(x = data$date, y= data[[i]], type = "l",  col = rgb(153/255, 0, 0, 0.5), lwd = 1)
}

for(i in 6:9){
  plot(x = data$date, y= data[[i]], type="l", xlab = "", ylab = "", main = colnames(val_set[i]),
       col = rgb(0, 0, 153/255, 0.5), font.main = 2)
  grid(NA, NULL,lty = 2)
  lines(x = data$date, y= data[[i]], type = "l",  col = rgb(0, 0, 153/255, 0.5), lwd = 1)
}

for(i in 10:12){
  plot(x = data$date, y= data[[i]], type="l", xlab = "", ylab = "", main = colnames(val_set[i]),
       col = rgb(51/255, 0, 102/255, 0.5), font.main = 2)
  grid(NA, NULL,lty = 2)
  lines(x = data$date, y= data[[i]], type = "l",  col = rgb(51/255, 0, 102/255, 0.5), lwd = 1)
}

dev.off()

# plot the processing data
png("/Users/82108/Desktop/processing_data.png", width = 2000, height = 1500, res = 300)
par(mfcol = c(4,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))

for(i in 2:5){
  plot(x = val_set$date_col, y= val_set[[i]], type="l", xlab = "", ylab = "", main = colnames(val_set[i]),
       col = rgb(153/255, 0, 0, 0.5), font.main = 2)
  grid(NA, NULL,lty = 2)
  lines(x = val_set$date_col, y= val_set[[i]], type = "l",  col = rgb(153/255, 0, 0, 0.5), lwd = 1)
  }

for(i in 6:9){
  plot(x = val_set$date_col, y= val_set[[i]], type="l", xlab = "", ylab = "", main = colnames(val_set[i]),
       col = rgb(0, 0, 153/255, 0.5), font.main = 2)
  grid(NA, NULL,lty = 2)
  lines(x = val_set$date_col, y= val_set[[i]], type = "l",  col = rgb(0, 0, 153/255, 0.5), lwd = 1)
}

for(i in 10:12){
  plot(x = val_set$date_col, y= val_set[[i]], type="l", xlab = "", ylab = "", main = colnames(val_set[i]),
       col = rgb(51/255, 0, 102/255, 0.5), font.main = 2)
  grid(NA, NULL,lty = 2)
  lines(x = val_set$date_col, y= val_set[[i]], type = "l",  col = rgb(51/255, 0, 102/255, 0.5), lwd = 1)
}

dev.off()





