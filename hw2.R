###HW2 Yifan Wang
library(ggplot2)
library(tidyverse)
library(dplyr)

##EX1
datind2009 <- read.csv("~/Desktop/Data/datind2009.csv")
hw2 <- data.frame("empstat" = datind2009$empstat,
                     "wage" = datind2009$wage,
                     "age"=datind2009$age)
hw2=hw2%>%filter(!is.na(wage), wage!=0, age>0) #remove NA and 0 in wage,and 0 in age

#1)
M = as.matrix(cbind(hw2$age,hw2$wage))
n = nrow(M)
C = diag(n) - matrix(1/n, n, n) #Centering matrix
Mc = C %*% M
S = t(Mc) %*% Mc / (n-1) #covariance matrix
E = solve(diag(x = sqrt(diag(S)), 2, 2, names = TRUE))
R = E%*%S%*%E #correlation matrix
correlation = R[1,2]
correlation

#2)
X = as.matrix(cbind(1,hw2$age))
Y = as.matrix(hw2$wage)
b_hat = solve(t(X)%*%X)%*%t(X)%*%Y
b_hat

#3a)
res = as.matrix(hw2$wage-b_hat[1]-b_hat[2]*hw2$age) # Calculate vector of residuals
n = nrow(hw2)
k = ncol(X)
VCV = 1/(n-k) * as.numeric(t(res)%*%res) * solve(t(X)%*%X) #Variance-Covariance Matrix
SE = sqrt(diag(VCV)) #Standard errors of the estimated coefficients
SE

#3b)
R    = 49 # number of bootstraps
nind = nrow(hw2)# number of individuals
nvar = 2  # number of variables
outs = mat.or.vec(R,nvar)
set.seed(123)
for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = hw2[samp,]
  X0 = as.matrix(cbind(1,dat_samp$age))
  Y0 = as.matrix(dat_samp$wage)
  b_hat0 = solve(t(X0)%*%X0)%*%t(X0)%*%Y0
  outs[i,] = b_hat0 #coeficient
}
sd_est0 = apply(outs,2,sd)
sd_est0
#####replicate 449 times
R    = 499 # number of bootstraps
nind = nrow(hw2)# number of individuals
nvar = 2  # number of variables
outs1 = mat.or.vec(R,nvar)
set.seed(123)
for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = hw2[samp,]
  X0 = as.matrix(cbind(1,dat_samp$age))
  Y0 = as.matrix(dat_samp$wage)
  b_hat0 = solve(t(X0)%*%X0)%*%t(X0)%*%Y0
  outs1[i,] = b_hat0 #coeficient
}
sd_est1 = apply(outs1,2,sd)
sd_est1

##EX2
#1)
datind2005 <- data.frame(read.csv("~/Desktop/Data/datind2005.csv", colClasses = c(idmen="character", idind="character")))
datind2006 <- data.frame(read.csv("~/Desktop/Data/datind2006.csv", colClasses = c(idmen="character", idind="character")))
datind2007 <- data.frame(read.csv("~/Desktop/Data/datind2007.csv", colClasses = c(idmen="character", idind="character")))
datind2008 <- data.frame(read.csv("~/Desktop/Data/datind2008.csv", colClasses = c(idmen="character", idind="character")))
datind2009 <- data.frame(read.csv("~/Desktop/Data/datind2009.csv", colClasses = c(idmen="character", idind="character")))
datind2010 <- data.frame(read.csv("~/Desktop/Data/datind2010.csv", colClasses = c(idmen="character", idind="character")))
datind2011 <- data.frame(read.csv("~/Desktop/Data/datind2011.csv", colClasses = c(idmen="character", idind="character")))
datind2012 <- data.frame(read.csv("~/Desktop/Data/datind2012.csv", colClasses = c(idmen="character", idind="character")))
datind2013 <- data.frame(read.csv("~/Desktop/Data/datind2013.csv", colClasses = c(idmen="character", idind="character")))
datind2014 <- data.frame(read.csv("~/Desktop/Data/datind2014.csv", colClasses = c(idmen="character", idind="character")))
datind2015 <- data.frame(read.csv("~/Desktop/Data/datind2015.csv", colClasses = c(idmen="character", idind="character")))
datind2016 <- data.frame(read.csv("~/Desktop/Data/datind2016.csv", colClasses = c(idmen="character", idind="character")))
datind2017 <- data.frame(read.csv("~/Desktop/Data/datind2017.csv", colClasses = c(idmen="character", idind="character")))
datind2018 <- data.frame(read.csv("~/Desktop/Data/datind2018.csv", colClasses = c(idmen="character", idind="character")))
datind=rbind(datind2005,datind2006,datind2007,datind2008,
             datind2009,datind2010,datind2011,datind2012,datind2013,
             datind2014,datind2015,datind2016,datind2017,datind2018)
ex2 = datind[c("year","wage","age","empstat")]
ex2=ex2%>%filter(!is.na(wage), wage!=0, age>=18) #remove NA and 0 in wage,and age less than 18
ex2$agecut<-cut(ex2$age, c(17,25,30,35,40,45,50,55,60,Inf))

#2) Plot the wage of each age group across years
ggplot(ex2, aes(x = factor(year), y = wage, color = factor(year))) + 
  geom_boxplot(outlier.colour=NA) + 
  coord_cartesian(ylim = c(0, 70000))+ facet_grid(. ~ agecut)+ labs(title= "Boxplot of wage distribution of each age group across years")

#3) time fixed effect
before=lm(wage~age , data = ex2)
before$coefficients[2]#before including time fixed effect
ex2$year=as.factor(ex2$year)
time = lm(wage ~ age + year -1, data = ex2)
time$coefficients[1] #coefficient decrease after including time fixed effect

###EX3
#1) Exclude all individuals who are inactive.
data2007=datind2007%>%filter(empstat!="Inactive"& empstat!="Retired",!is.na(wage), wage!=0, age>=18) #remove NA and 0 in wage,and age less than 18

#2)Write a function that returns the likelihood of the probit of being employed.
data2007=data2007%>% mutate(employed=case_when(empstat=="Employed"~1,
                          TRUE~0))
reg1 = glm(employed~age,family = binomial(link = "probit"),data=data2007)
summary(reg1)

x = data2007$age
y = data2007$employed
table(y)

probitlike = function(beta,x,y)
{
  x_b           = beta[1] + beta[2]*x
  p             = pnorm(x_b)
  p[p>0.999999] = 0.999999
  p[p<0.000001] = 0.000001
  like           = y*log(p) + (1-y)*log(1-p)
  return(-sum(like))
}
# test if the function is correct
test = reg1$coefficients
probitlike(test,x,y)
logLik(reg1)

#3)Optimize the model and interpret the coefficients.
set.seed(4)
beta = runif(2)
res  = optim(beta,fn=probitlike,method="BFGS",x=x,y=y,hessian=TRUE)
compare = cbind(summary(reg1)$coefficients[, 1],res$par)
colnames(compare) = c("GLM: est","own: est")
compare
#Interpretation: People with older age are more likely to participate in labor market

#4)Can you estimate the same model including wages as a determinant of labor market participation?
#No, becasue most people who are unemployed have 0 wage, and we should drop wage=0, so the result is not accurate.

##EX4
ex4=rbind(datind2005,datind2006,datind2007,datind2008,
             datind2009,datind2010,datind2011,datind2012,datind2013,
             datind2014,datind2015)
#1)exclude all individuals who are inactive.
active = ex4%>%filter(empstat!="Inactive"& empstat!="Retired",!is.na(wage), wage!=0, age>=18) #remove NA and 0 in wage,and age less than 18

#2)Write and optimize the probit, logit, and the linear probability models.
#build binary y variable
dataex4=active%>% mutate(employed=case_when(empstat=="Employed"~1,TRUE~0))
y=dataex4$employed
x=dataex4$age
#build year dummy variables
dataex4=dataex4%>% mutate(z1=case_when(year=="2006"~1,TRUE~0))%>% mutate(z2=case_when(year=="2007"~1,TRUE~0))%>% mutate(z3=case_when(year=="2008"~1,TRUE~0))%>% mutate(z4=case_when(year=="2009"~1,TRUE~0))%>% mutate(z5=case_when(year=="2010"~1,TRUE~0))%>% mutate(z6=case_when(year=="2011"~1,TRUE~0))%>% mutate(z7=case_when(year=="2012"~1,TRUE~0))%>% mutate(z8=case_when(year=="2013"~1,TRUE~0))%>% mutate(z9=case_when(year=="2014"~1,TRUE~0))%>% mutate(z10=case_when(year=="2015"~1,TRUE~0))
z1=dataex4$z1
z2=dataex4$z2
z3=dataex4$z3
z4=dataex4$z4
z5=dataex4$z5
z6=dataex4$z6
z7=dataex4$z7
z8=dataex4$z8
z9=dataex4$z9
z10=dataex4$z10

#probit  model
reg1 = glm(employed~age+as.factor(year),family = binomial(link = "probit"),data=dataex4)
regg=glm(y~x+z1+z2+z3+z4+z5+z6+z7+z8+z9+z10,family = binomial(link = "probit"),data=dataex4)
summary(regg)
summary(reg1)
probitlike = function(beta,x,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,y)
{
  x_b           = beta[1]+beta[2]*x+beta[3]*z1+beta[4]*z2+beta[5]*z3+beta[6]*z4+beta[7]*z5+beta[8]*z6+beta[9]*z7+beta[10]*z8+beta[11]*z9+beta[12]*z10
  p             = pnorm(x_b)
  p[p>0.999999] = 0.999999
  p[p<0.000001] = 0.000001
  like           = y*log(p) + (1-y)*log(1-p)
  return(-sum(like))
}
# test if the function is correct
test = reg1$coefficients
probitlike(test,x,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,y)
logLik(reg1)

#optimize coeficient
set.seed(4)
beta = runif(12)
res1 = optim(beta,fn=probitlike,method="BFGS",x=x,z1=z1,z2=z2,z3=z3,z4=z4,z5=z5,z6=z6,z7=z7,z8=z8,z9=z9,z10=z10,y=y,hessian=TRUE)
compareprobit = cbind(summary(reg1)$coefficients[,1],res1$par)
colnames(compareprobit) = c("GLM probit est","own probit est")
compareprobit

#logit  model
reg2 = glm(employed~age+as.factor(year),family = binomial(link = "logit"),data=dataex4)

logitlike = function(beta,x,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,y)
{
  x_b          = beta[1]+beta[2]*x+beta[3]*z1+beta[4]*z2+beta[5]*z3+beta[6]*z4+beta[7]*z5+beta[8]*z6+beta[9]*z7+beta[10]*z8+beta[11]*z9+beta[12]*z10
  p            = exp(x_b)/(1+exp(x_b))
  p[p>0.999999] = 0.999999
  p[p<0.000001] = 0.000001
  like           = y*log(p) + (1-y)*log(1-p)
  return(-sum(like))
}
# test if the function is correct
test = reg2$coefficients
logitlike(test,x,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,y)
logLik(reg2)
#optimize coeficient
set.seed(4)
beta = runif(12)
res2 = optim(beta,fn=logitlike,method="BFGS",x=x,z1=z1,z2=z2,z3=z3,z4=z4,z5=z5,z6=z6,z7=z7,z8=z8,z9=z9,z10=z10,y=y,hessian=TRUE)
comparelogit = cbind(summary(reg2)$coefficients[, 1],res2$par)
colnames(comparelogit) = c("GLM logit est","own logit est")
comparelogit 

#linear probability model
reg3=lm(employed~age+factor(year), data=dataex4)
X = as.matrix(cbind(1,x,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10))
Y = as.matrix(dataex4$employed)
b_hat = solve(t(X)%*%X)%*%t(X)%*%Y
comparelinear = cbind(summary(reg3)$coefficients[, 1],b_hat)
colnames(comparelinear) = c("LM est","own linear est")
comparelinear 

#3)Interpret and compare the estimated coefficients. How significant are they?
#For probit model, beta_age= 0.01634999, statistically significant. Interpretation: holding everything else constant, people with one year older are more likely to participate in labor market
  
#For logit model, beta_age= 0.01634999, statistically siginficant. Interpretation: holding everything else constant, people with one year older are more likely to participate in labor market
  
#For linear probability model, beta_age= 0.002257668, statistically significant. Interpretation: holding everything else constant, people with one year older are more likely to participate in labor market
  
#For all three models, the coefiecients for year 2006, 2008, and 2011 are not statistically significant. The interpretation for significant year fixed effects are: compare to the basegroup (year 2005),holding everything else constant, people in year 2006-2008 are more likely to participate in labor market; people in year 2009-2015 are less likely to participate in labor market.


#EX5
#1)Compute the marginal effect of the previous probit and logit models.
#calculate mean of each varibles
X <- cbind(1,x,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10)　
Xmean <- apply(X, 2, mean)

#probit marginal effect evaluated at the mean
ME_probit <- res1$par* as.vector(dnorm(Xmean %*% res1$par))
ME_probit

#logit marginal effect evaluated at the mean
ME_logit<- res2$par * as.vector(dlogis(Xmean %*% res2$par))
ME_logit


#2)Construct the standard errors of the marginal effects
#probit model
ntry = 5
out = mat.or.vec(ntry,12)
for (i0 in 1:ntry)
{
  beta    = runif(12)
  res1 = optim(beta,fn=probitlike,method="BFGS",x=x,z1=z1,z2=z2,z3=z3,z4=z4,z5=z5,z6=z6,z7=z7,z8=z8,z9=z9,z10=z10,y=y,hessian=TRUE)
  X <- cbind(1,x,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10)　
  Xmean <- apply(X, 2, mean)
  out[i0,] = res1$par * as.vector(dnorm(Xmean%*% res1$par)) 
}
st_error<-apply(out, 2, sd)
st_error
#logit model
ntry = 5
out = mat.or.vec(ntry,12)
for (i0 in 1:ntry)
{
  beta    = runif(12)
  res2 = optim(beta,fn=logitlike,method="BFGS",x=x,z1=z1,z2=z2,z3=z3,z4=z4,z5=z5,z6=z6,z7=z7,z8=z8,z9=z9,z10=z10,y=y,hessian=TRUE)
  X <- cbind(1,x,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10)　
  Xmean <- apply(X, 2, mean)
  out[i0,] = res2$par * as.vector(dlogis(Xmean%*% res2$par)) 
}
st_error<-apply(out, 2, sd)
st_error

