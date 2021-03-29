library(gclus)
library(ggplot2)
library(car)

rm(list=ls())

#--------------------------------------------------
# Part 1

data(ozone)


par(mar=c(1,1,1,1))

cpairs(ozone)
boxplot(ozone)

par(mfrow = c(2,2))


fit1 <- lm(Ozone ~. , data = ozone)
Anova(fit1, type = 'III')
summary(fit1)
AIC(fit1)

fit2 <- lm(Ozone ~ . - Pres, data = ozone)
summary(fit2)
AIC(fit2)

fit3 <- lm(Ozone ~ . - Pres - Wind, data = ozone)
summary(fit3)
AIC(fit3)

fit4 <- lm(Ozone ~ . - Pres - Wind - 1, data = ozone)
summary(fit4)
AIC(fit4)

fit5 <- lm(Ozone ~ . - Pres - Wind - Vis - 1, data = ozone)
summary(fit5)
AIC(fit5)
plot(fit5)


fit_log1 <- lm(log(Ozone) ~ ., data = ozone)
summary(fit_log1)
AIC(fit_log1)

fit_log2 <- lm(log(Ozone) ~ . - InvTmp, data = ozone)
summary(fit_log2)
AIC(fit_log2)

fit_log3 <- lm(log(Ozone) ~ . - InvTmp - Wind, data = ozone)
summary(fit_log3)
AIC(fit_log3)

fit_log4 <- lm(log(Ozone) ~ . - InvTmp - Wind - Hgt, data = ozone)
summary(fit_log4)
AIC(fit_log4)

fit_log5 <- lm(log(Ozone) ~ . - InvTmp - Wind - Hgt - Pres, data = ozone)
summary(fit_log5)
AIC(fit_log5)

fit_log6 <- lm(log(Ozone) ~ . - InvTmp - Wind - Hgt - Pres - Vis, data = ozone)
summary(fit_log6)
AIC(fit_log6)
plot(fit_log6)






fit3 <- glm(Ozone ~ ., data = ozone, family = poisson())
Anova(fit3, type = 'III')$AIC

fit3 <- glm(log(Ozone) ~ ., data = ozone, family = poisson())
summary(fit3)

fit4 <- glm(Ozone ~ ., data = ozone, family = Gamma(link="log"))
summary(fit4)



#------------------------------------------------------------
# Part 2
rm(list=ls())
data <- read.csv('data/dat_count.csv', sep = ";") # , stringsAsFactors = T
data[data$sex == 'female','sex'] = 0
data[data$sex == 'male','sex'] = 1

#omitting subjId & day
data <- data[3:8]
data$clo <- as.factor(data$clo)
#data$nobs <- as.factor(data$nobs)
data

fit1 <- glm(clo ~ ., data = data, family = binomial(link = "logit"))
Anova(fit1)





