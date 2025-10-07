library(wooldridge)
data(hprice1)
names(hprice1)
model1<-lm(price~sqrft+bdrms,data=hprice1)
summary(model1)
coef(model1)["bdrms"] + coef(model1)["sqrft"] * 140
r2 <- summary(model1)$r.squared

new_obs <- data.frame(sqrft = 2438, bdrms = 4)
pred_price <- predict(model1, newdata = new_obs)
pred_price
#alternatively
p_hat<-coef(model1)[1]+coef(model1)[2]*2438+coef(model1)[3]*4
actual_price <- 300
residual <- actual_price - pred_price
residual
#How many additional square feet are needed to offset the loss of one bedroom, 
#keeping predicted price constant?

add_sqft <-coef(model1)[3]/ coef(model1)[2]
add_sqft

