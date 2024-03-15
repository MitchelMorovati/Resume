rm(list=ls())  

library(zoo)


data_gas = read.csv("data_gasoline_prices.csv", header = T, sep = ",", stringsAsFactors = T)

week = data.frame(week = 12,
                  price = NA)

# create a predicted dataframe
pred_tablet = rbind(data_gas, week)


k = 3
ma3 = rollmean(data_gas$price, k)

ma3

# put the moving average sequence to the predicted dataframe
pred_tablet$ma3[(k+1):nrow(pred_tablet)] = ma3 

# now calculate moving average with k = 5
k = 5
ma5 = rollmean(data_gas$price, k)

# put the moving average sequence to the predicted dataframe
pred_tablet$ma5[(k+1):nrow(pred_tablet)] = ma5



# forecast week11's sale
# when use k=3 moving average
pred_tablet$ma3[11]

# when use k=5 moving average
pred_tablet$ma5[11]



# plot the time series
plot(price ~ week, data = data_gas, type = "l", xlim = c(0,12))

# add the moving average lines
lines(pred_tablet$ma3, type = "l", col = "red")
lines(pred_tablet$ma5, type = "l", col = "blue")

# add legend
legend("topright", c("Time Series", "Moving Average k=3", "Moving Average k=5"), lty = 1, 
       cex = 0.5, col = c("black", "red", "blue"))




### forecast using exponential smoothing 
# create a time series object from dataset
ts_gas = ts(data_gas$price)

# create exponential smoothing, set smoothing constant alpha = 0.3
exps2 = HoltWinters(ts_gas, alpha = 0.3, beta = F, gamma = F)

# take a look at the exponential smoothing fitted value sequence
exps2$fitted

# put fitted values from exponential smoothing back to dataframe
pred_tablet$exps2 = NA
pred_tablet$exps2[2:11] = exps2$fitted[,1]

# make forecast of week12
pred_tablet$exps2[12] = predict(exps2, n.ahead = 1)



# create exponential smoothing, set smoothing constant alpha = 0.6
exps6 = HoltWinters(ts_gas, alpha = 0.6, beta = F, gamma = F)

# put fitted values from exponential smoothing back to dataframe
pred_tablet$exps6 = NA
pred_tablet$exps6[2:11] = exps6$fitted[,1]

# make forecast of week18
pred_tablet$exps6[12] = predict(exps6, n.ahead = 1)


# plot the time series and exponential smoothing
plot(price ~ week, data = data_gas, type = "l", xlim = c(0,12))
lines(pred_tablet$exps2, type = "l", col = "red")
lines(pred_tablet$exps6, type = "l", col = "blue")
legend("topright", c("Time Series", "Exponential Smoothing alpha=0.3", "Exponential Smoothing alpha=0.6"), lty = 1, 
       cex = 0.5, col = c("black", "red", "blue"))


# plot time series, moving average and exponential smoothing
plot(price ~ week, data = data_gas, type = "l", xlim = c(0,12))
lines(pred_tablet$ma3, type = "l", col = "red")
lines(pred_tablet$exps6, type = "l", col = "blue")
legend("topright", c("Time Series", "Moving Average k=3", "Exponential Smoothing alpha=0.6"), lty = 1, 
       cex = 0.5, col = c("black", "red", "blue"))

