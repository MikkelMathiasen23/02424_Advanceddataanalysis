getwd()
dat = read.csv(file="data/clothingFullAss03.csv")
dat$sex_male = as.numeric(as.factor(dat$sex))-1
dat$sex_female = abs(as.numeric(as.factor(dat$sex))-2)

library(TMB)
parameters <- list(u=rep(0,length(unique(dat$subjId))),
                   v = rep(0,length(unique(dat$subDay))),
                   gamma = rep(0,length(unique(dat$subjId))),
                   sigma_u = 1,
                   sigma_v = 1,
                   sigma_G = 1,
                   sigma = 1,
                   alpha = 1,
                   beta = 1,
                   mu = 1
)
parameters
tmp_df = unique(dat[,c('sex',"sex_male","subjId")])
tmp_df2 = unique(dat[,c('sex',"sex_male","subDay","subjId")])
new_dat = list('clo'=dat$clo,
               'subjId' = dat$subjId,
               'subjId_day' = dat$subDay,
               'sex_male' = dat$sex_male,
               
               'subjId_u'=tmp_df$subjId,
               'alpha_u_sex_male'= tmp_df$sex_male,
               
               'alpha_v_sex_male'= tmp_df2$sex_male,
               'subjId_v' = tmp_df2$subjId
)

nobs = length(new_dat$clo)
for(k in 1:nobs){
  i = new_dat$subjId[k]+1
  j = new_dat$subjId_day[k]+1
  print(paste("i",i,"u",parameters$u[i],"v",parameters$v[j]))
}
new_dat

compile("test.cpp")
dyn.load(dynlib("test"))
## Define objective function
obj <- MakeADFun(data = new_dat,
                 parameters = parameters,
                 random = c("u","v","gamma"),
                 DLL = "test"
)
obj$fn(obj$par)