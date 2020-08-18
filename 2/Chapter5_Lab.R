# SMiB - Lab 5
library(ISLR)
set.seed(1)
train=sample(392,196)

### VALIDATION SET APPROACH
# Linear regression
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
# MSE for linear regression:
mean((mpg-predict(lm.fit,Auto))[-train]^2)

# Quadratic regression
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
# MSE for quadratic regression:
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

# Cubic regression
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
# MSE for cubic regression:
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Use a different training set
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)

# Linear regression
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
# MSE for linear regression:
mean((mpg-predict(lm.fit,Auto))[-train]^2)

# Quadratic regression
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
# MSE for quadratic regression:
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

# Cubic regression
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
# MSE for cubic regression:
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

### CONCLUSION:
# No measurable benefit to using cubic over quadratic regression


### LOOCV APPOACH
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
# Numbers should be the same in a CV
cv.err$delta

cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
# Error changes in higher order polynomials
cv.error


### K-FOLD CROSS VALIDATION
set.seed=(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
# Error change in higher order polynomials
cv.error.10

### BOOTSTRAP
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio, alpha.fn, R=1000)

#Validate linear regression
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:1392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

# Compute stderr of 1000 bootstrap estimates
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower,data=Auto))$coeff


# EXERCISES
# Conceptual: 1,4
# Applied: 5,6,8

# 1


# 4
# We can estimate the standard deviation of our prediction by bootstrapping.
# By randomly sampling from our existing sample with replacement,
# We can determine the standard deviation

# 5
attach(Default)
set.seed(1)
# a)
fit.glm=glm(default~income+balance,data=Default,family="binomial")
# b)
# i
train=sample(dim(Default)[1],dim(Default)[1]/2)
# ii
fit.glm=glm(default~income+balance,data=Default,family="binomial",subset=train)
summary(fit.glm)
# iii
probs=predict(fit.glm,Default[-train, ],type="response")
pred.glm=rep("No",length(probs))
pred.glm[probs>0.5] = "Yes"
pred.glm
# iv: test error rate
mean(pred.glm != Default[-train, ]$default)

# c)
cv.error=rep(0,3)
for (i in 1:3){
  train=sample(dim(Default)[1],dim(Default)[1]/2)
  fit.glm=glm(default~income+balance,data=Default,family="binomial",subset=train)
  probs=predict(fit.glm,Default[-train, ],type="response")
  pred.glm=rep("No",length(probs))
  pred.glm[probs>0.5] = "Yes"
  cv.error[i]=mean(pred.glm != Default[-train, ]$default)
}
cv.error
# Sorta close to the original?

# d)
train=sample(dim(Default)[1],dim(Default)[1]/2)
fit.dummy=glm(default~income+balance+student,data=Default,family="binomial")
probs=predict(fit.dummy,Default[-train, ],type="response")
pred.glm=rep("No",length(probs))
pred.glm[probs>0.5]="Yes"
mean(pred.glm != Default[-train, ]$default)
# no real difference; still close to the original


# 6
set.seed(4)
# a)
glm.fit=glm(default~income+balance,data=Default,family="binomial")
summary(glm.fit)

# b)
boot.fn=function(data,index)
  return(coef(glm(default~income+balance,data=data,family="binomial", subset=index)))

# c)
boot(Default, boot.fn, 1000)

# d)
# Standard errors hardly differ


# 8
# a)
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
# n = 100, p = 2

# b)
plot(x,y)

# c)
set.seed=(17)
Data=data.frame(x,y)
cv.error.4=rep(0,4)
for (i in 1:4){
  glm.fit=glm(y~poly(x,i),data=Data)
  cv.error.4[i]=cv.glm(Data,glm.fit,K=10)$delta[1]
}
cv.error.4

# d)
set.seed(1)
Data=data.frame(x,y)
cv.error.4=rep(0,4)
for (i in 1:4){
  glm.fit=glm(y~poly(x,i),data=Data)
  cv.error.4[i]=cv.glm(Data,glm.fit,K=10)$delta[1]
}
cv.error.4
# Similar;

# e)
# Quadratic had the smallest error, it was expected

# f) 
glm.fit.4=glm.fit=glm(y~poly(x,4),data=Data)
summary(glm.fit.4)
# Polynomials > 2 had an insignificant and small effect on fitting the data
