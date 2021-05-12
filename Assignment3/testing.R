ll_Y_given_gamma_Q6<- function(pars,gamma,alpha_sex="male"){
  #print(c(pars[1:2],exp(pars[3]+pars[4:6]) ))
  #print(pars)
  # ML estimation:
  mu = pars["mu"]
  sex_male = pars["sex_male"]
  beta=c(mu,sex_male)
  alpha = pars['alpha']
  
  
  ll=0
  for(i in unique(dat$subjId)){
    #print(i)
    df = dat[dat$subjId==i,]
    df$sex
    if(alpha_sex=="male"){
      sex = df$sex_male[1]
    }
    if(alpha_sex=="female"){
      sex = df$sex_female[1]
    }
    
    #print(sex)
    n = nrow(df)
    days = unique(df$day)
    
    if(df$sex[1]=="male"){
      x = cbind(1,rep(1,n))
    }else{
      x = cbind(1,rep(0,n))
    }
    
    sig = diag(n)*exp(sex*alpha+2*pars["sigma"])+exp(sex*alpha+2*pars["sigma_s"])
    for(k in days){
      idx = which(df$day==k)
      sig[idx,idx] = sig[idx,idx]+exp(sex*alpha+2*pars["sigma_sub_day"])
    }
    
    
    y = df$clo
    tmp = dmvn(y,x%*%beta,sig*exp(-gamma[i+1]),log=T)
    #tmp = dmvnorm(y,x%*%beta,sig,log=T)
    #print(tmp)
    ll = ll+tmp
  }
  return(ll)
}

ll_Y_given_gamma<- function(pars,gamma,alpha_sex="male"){
  #print(c(pars[1:2],exp(pars[3]+pars[4:6]) ))
  #print(pars)
  # ML estimation:
  mu = pars["mu"]
  sex_male = pars["sex_male"]
  beta=c(mu,sex_male)
  alpha = pars['alpha']
  
  
  ll=0
  for(i in unique(dat$subjId)){
    #print(i)
    df = dat[dat$subjId==i,]
    df$sex
    if(alpha_sex=="male"){
      sex = df$sex_male[1]
    }
    if(alpha_sex=="female"){
      sex = df$sex_female[1]
    }
    
    #print(sex)
    n = nrow(df)
    days = unique(df$day)
    
    if(df$sex[1]=="male"){
      x = cbind(1,rep(1,n))
    }else{
      x = cbind(1,rep(0,n))
    }
    
    sig = diag(n)*exp(sex*alpha+2*pars["sigma"])+exp(sex*alpha+2*pars["sigma_s"])
    for(k in days){
      idx = which(df$day==k)
      sig[idx,idx] = sig[idx,idx]+exp(sex*alpha+2*pars["sigma_sub_day"])
    }
    
    
    y = df$clo
    tmp = dmvn(y,x%*%beta,sig/gamma[i+1],log=T)
    #tmp = dmvnorm(y,x%*%beta,sig,log=T)
    #print(tmp)
    ll = ll+tmp
  }
  return(ll)
}

gamma_Q5 <-function(gamma,phi){
  ll = dgamma(gamma,shape=phi,rate=phi,log=T)
  return(ll)
}

gamma_Q6 <-function(gamma,sigma_G){
  dnorm(gamma,mean = 0 ,sd = sigma_G,log=T)
}

get_gamma_Q5<-function(gamma,pars){
  marginal = ll_Y_given_gamma(pars = pars, gamma = gamma)
  gamma_dist = gamma_Q5(gamma = gamma,phi = exp(pars["phi"]))
  ll = marginal+sum(gamma_dist)
  return(-ll)
}

get_gamma_Q6<-function(gamma,pars){
  #print(gamma)
  marginal = ll_Y_given_gamma_Q6(pars = pars, gamma = gamma)
  gamma_dist = gamma_Q6(gamma = gamma,sigma_G = exp(pars["sigma_G"]))
  ll = marginal+sum(gamma_dist)
  return(-ll)
}
gamma = rep(1,length(unique(dat$subjId)))
Q5_gamma = nlminb(gamma,get_gamma_Q5,pars = out_par,lower = 1e-9)
names(opt_par6)[c(2,3,5)] = names(out_par)[c(2,5,4)]

Q6_gamma = nlminb(gamma,get_gamma_Q6,pars = opt_par6)
exp(test2$par)-test$par


