rm(list=ls())

library(broom)

broke <-read.csv("data_broker.csv", header =  T, sep = ',', stringsAsFactors = T)

str(broke)

summary(broke)

broke_reg <- lm(overall_sat ~ sw_speed + sw_tradePrice, data = broke)

summary(broke_reg)

broke_reg$coefficients

neat_output <- tidy(broke_reg)

betahat1 <- neat_output$estimate[2]
betahat1

betahat2 <- neat_output$estimate[3]
betahat2

bse1 <- neat_output$std.error[2]
bse1

bse2 <- neat_output$std.error[3]
bse2

uci1 <- betahat1 + 1.96*bse1
lci1 <- betahat1 - 1.96*bse1
uci1
lci1


uci2 <- betahat2 + 1.96*bse2
lci2 <- betahat2- 1.96*bse2
uci2
lci2


credit <-read.csv("data_credit.csv", header =  T, sep = ',', stringsAsFactors = T)


credit_reg <- lm(rating ~ income + cards + age + education + male + student + married, data = credit)
summary(credit_reg)


credit_reg2 <- lm(rating ~ income + cards + age,data = credit)
summary(credit_reg2)
Ex1 <- data.frame(income = 125, cards = 2, age = 40)
Ex1Predict <- predict(credit_reg2,Ex1)
Ex1Predict
