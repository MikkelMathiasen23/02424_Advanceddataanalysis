library(ggplot2)
library(lmtest)
library(car)

df1=read.csv(file="./Data/clothingFull.csv")
df2=read.csv(file="./Data/clothingSum.csv")

df3 = df2[,c(3:6)]


#Question 2:
fit2 = lm( clo ~ tOut + tInOp + sex + tInOp*sex,data = df3)
Anova(fit2,type = 'III')

df3$residual = resid(fit2)

ggplot(df3, aes(sample = residual, colour = sex)) +
  stat_qq() +
  stat_qq_line()

ggplot(df3, aes(x = residual, colour = sex)) +
  geom_histogram()+ facet_wrap(~sex)

'fit3 = lm( clo ~ tOut + tInOp + sex,data = df3)
summary(fit3)
'

lrtest(fit2, fit3)

df3$residual = NA
df3[df3$sex == 'female', 5 ] = resid(fit2)[df3$sex == 'female']
df3[df3$sex == 'male', 5 ] = resid(fit2)[df3$sex == 'male']
ggplot(data = df3, aes(x = sex, y= residual))+ geom_boxplot() + geom_jitter(width = 0.3)

ggplot(data = df3, aes(x = tInOp, y= residual, col = sex))+ geom_point() 

ggplot(data = df3, aes(x = tOut, y= residual, col = sex))+ geom_point()
ggplot(data = df3, aes(x = clo, y= residual, col = sex))+ geom_point()

ggplot(data = df3, aes(x = tOut, y= residual, col =as.factor(df2$day) ,group = df2$subjId))+ geom_point()+geom_line()


### Question 4:
fit2f = lm( clo ~ tOut + tInOp   ,data = df3[df3$sex == 'female',])
summary(fit2f)
fit2m = lm( clo ~ tOut + tInOp  ,data = df3[df3$sex == 'male',])
summary(fit2m)


df3$residual = NA
df3[df3$sex == 'female', 5 ] = resid(fit2f)
df3[df3$sex == 'male', 5 ] = resid(fit2m)
ggplot(data = df3, aes(x = sex, y= residual))+ geom_boxplot() + geom_jitter(width = 0.3)
frac = var(resid(fit2f))/var(resid(fit2m))


# N(mu, sigma^2) sigma different between two gender 
res = resid(fit2)
res[df3$sex == 'female'] = res[df3$sex == 'female']/frac
qqnorm(res)
qqline(res)


##Question 5:
#Add also quantiles for tInOp. 
#Only done for mean right now 
tmp_male=data.frame(clo=NA,tOut=seq(min(df2$tOut),max(df2$tOut),0.1),tInOp=mean(df2$tInOp),sex="male")
tmp_female=data.frame(clo=NA,tOut=seq(min(df2$tOut),max(df2$tOut),0.1),tInOp=mean(df2$tInOp),sex="female")
new=rbind(tmp_male,tmp_female)

conf = predict(fit2,tmp_male,interval = 'confidence')
pred = predict(fit2, tmp_male,interval = 'prediction')
plot(tmp_male$tOut, conf[,1], col = 'blue', type = 'l',ylim = c(min(pred), max(pred)), ylab = 'clo', xlab = 'tOut')
lines(tmp_male$tOut,conf[,2], col = 'red', lty = 2)
lines(tmp_male$tOut,conf[,3], col = 'red', lty = 2)
lines(tmp_male$tOut,pred[,2], col = 'green', lty = 2)
lines(tmp_male$tOut,pred[,3], col = 'green', lty = 2)
idx = df2$sex == 'male'
points(df2$tOut[idx], df2$clo[idx], pch = 25)

conf = predict(fit2,tmp_female,interval = 'confidence')
pred = predict(fit2, tmp_female,interval = 'prediction')
plot(tmp_male$tOut, conf[,1], col = 'blue',lwd = 2, type = 'l',ylim = c(min(pred), max(pred)), ylab = 'clo', xlab = 'tOut')
lines(tmp_female$tOut,conf[,2], col = 'red',lwd = 2, lty = 2)
lines(tmp_female$tOut,conf[,3], col = 'red',lwd = 2, lty = 2)
lines(tmp_female$tOut,pred[,2], col = 'green',lwd = 2, lty = 2)
lines(tmp_female$tOut,pred[,3], col = 'green',lwd = 2, lty = 2)
idx2 = df2$sex == 'female'
points(df2$tOut[idx2], df2$clo[idx2], pch = 25)



##### Question 6
df3$residual_2 = resid(fit2)
df3$idx = 1:nrow(df3)
ggplot(data = df3, aes(x = idx, y = residual_2, col = as.factor(df2$subjId))) + geom_point() + geom_line() + facet_wrap(~sex)
#Can't remove subjId in final model 





################################################################################
### Part B
################################################################################

#Question 1 
#Sex as this result in singular matrix 

df2$subjId = as.character(df2$subjId)
fitB = lm( clo ~ tOut*tInOp+subjId,data = df2)
summary(fitB)

fit2B = lm( clo ~ tOut+tInOp+subjId,data = df2)
summary(fit2B)

fit3B = lm( clo ~ tOut+subjId,data = df2)
summary(fit3B)
plot(fit3B)
#Eventuelt plot coefficienter for hver subjId

qqnorm(resid(fit3B))                   
qqline(resid(fit3B))


#Question 2: 
hist(fit3B$coefficients[3:length(fit3B$coefficients)], nclass= 12)
l = length(fit3B$coefficients)

qqnorm(fit3B$coefficients[3:length(fit3B$coefficients)])
qqline(fit3B$coefficients[3:length(fit3B$coefficients)])


std_coef = sqrt(diag(vcov(fit3B)))[3:l]
coef_est = fit3B$coefficients[3:l]

df_coef = data.frame(cbind(coef_est, confint(fit3B)[3:l,]))
df_coef$names =rownames(df_coef)
df_coef$sex = NaN
library(stringr)

for (i in 1:(l-2)){
  
  df_coef$sex[i] = df2$sex[str_extract(df_coef$names[i],"([0-9]{1,3})")==df2$subjId] 
}

df_coef$names = as.numeric(str_extract(df_coef$names,"([0-9]{1,3})"))
df_coef = df_coef[order(as.numeric(df_coef$names)),]
df_coef$names = as.factor(df_coef$names)
levels(df_coef$names)


ggplot(data = df_coef, aes(x = names, y = coef_est, color = sex)) + 
  geom_pointrange(aes(ymin=X2.5.., ymax=X97.5..)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Question 3: 


###############################################################################
#Part C
###############################################################################
df1$subjId = as.factor(df1$subjId)
df1$day = as.factor(df1$day)
df1$obs.no = as.factor(df1$obs.no)

fitC = lm( clo ~ tInOp  + tOut+subjId +day + obs.no ,data = df1)
summary(fitC)


qqnorm(resid(fitC))
qqline(resid(fitC))



# Argue for Weighted analysis - question + how to fit it? 
# Part B - should we use mixed linear models with SubjId as random effect? 
# Visualization of model parameters - should we enforce distribution on the model parameters? 
# Part C: should we fit a new model after looking at the residuals? 

