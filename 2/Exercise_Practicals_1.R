# Exercise Practicals
library("boot")
library("ISLR")

results=rep(0,10)
for (i in 0:10){
  set.seed(i)
  train=sample(392,196)
  test=Auto[-train,]
  lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
  predictions=predict(lm.fit,test)
  results[i]=mean((test$mpg-predictions)^2)  
}
results


# Polynomial regression
set.seed(17)
testMSE=rep(0,6)
trainMSE=rep(0,6)
for(i in 1:6){
  lm.fit=lm(mpg~poly(horsepower,i),data=Auto,subset=train)
  testMSE[i]=mean((test$mpg-predict(lm.fit,test))^2)
  trainMSE[i]=mean((Auto$mpg[train]-predict(lm.fit))^2)
}

plot(testMSE,type="b",ylim=c(min(testMSE,trainMSE),max(testMSE,trainMSE))) 
lines(trainMSE, type="b",col=2)

# They diverge because the model gets more complex as more predictors are added
# A quadratic is probably the most likely model because their MSEs are closest together

loocv.error=rep(0,6)
cv10.error=rep(0,6)
for (i in 1:6){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  loocv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
  cv10.error[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
plot(loocv.error,type="b",ylim=c(min(loocv.error,cv10.error),max(loocv.error,cv10.error))) 
lines(cv10.error, type="b",col=2)




#BOOTSTRAP
x <- seq(-4, 4, length=100)
hx <- dt(x,1.1)
hx2 <- dnorm(x,sd=1)
plot(hx2~x,type="l")
lines(hx~x,col=2)
set.seed(3)
x = rnorm(1000)#some values, normal distribution around 0,sd=1
y = 1 + x + rt(1000,1.1) #simple relation, added noise is broad tailed! plot(x,y)
#broad tails give outliers
model = lm(y~x)
summary(model)


bootfun = function(data,b,formula){
  # b are the bootstrap sample indices
  d = data[b,] 
  return(lm(d[,1]~d[,2], data = d)$coef[2]) # thats the beta1 coefficient
}
result=boot(data=data.frame(y,x), statistic=bootfun, R=1000)
result

summary(model)

plot(result)

boot.ci(result, index=1, type=c("bca"))#estimated confidence interval for bootstrap
confint(model)#confidence interval for linear model



