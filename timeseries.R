
## A phase plot:
plot(p2016$pollen, lag(p2016$pollen, 1), cex = .8, col = "blue",
     main = "Lag plot of Atlanta Pollen Count")
abline(coef = c(0,1))


z0=ts(matrix(p2016$pollen),start = 75, end = 125, names = c("pollen"))

(fit1 <- arima(z0, c(1, 0, 0)))

nobs(fit1)
tsdiag(fit1)
(fit3 <- arima(z0, c(3, 0, 0)))  
tsdiag(fit3)

(fit11 <- arima(z0, c(1, 0, 1)))  
tsdiag(fit11)

(fit31 <- arima(z0, c(1, 1, 0)))  
tsdiag(fit31)

BIC(fit1, fit3, fit11, fit31)

# 
# ## compare a whole set of models; BIC() would choose the smallest
# AIC(fit1, arima(presidents, c(2,0,0)),
#     arima(presidents, c(2,0,1)), # <- chosen (barely) by AIC
#     fit3, arima(presidents, c(3,0,1)))
# 
# ## An example of ARIMA forecasting:
# predict(fit3, 3)
