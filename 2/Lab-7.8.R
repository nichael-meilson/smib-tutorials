# Lab 7.8.1
library(ISLR)
attach(Wage)
# Predict age using a 4th degree polynomial
# Each column is a linear combination of age, age^2, age^3, age^4
fit = lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

# Using poly() to get powers directly
# use 'raw=T' 
# effects the coeff estimates, but not the fitted vals
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))

# We now create a grid of values for age at which we want predictions
# predict() specifies that we want standard error as well
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# Plot and add to 4th-degree polynomial
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))

#ANOVA to test null hypothesis 
# in order to use, M1 & M2 need to be nested models
# predictors in M1 must be a subset of predictors in M2
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
# p comparing M1 & M2 is ~0; linear fit is not sufficient
# p comparing M2 & M3 is 0.0017; quad. fit is not significant
# p comparing M3 & M4 is 0.05
# p comparing M4 & M5 is 0.37
# therefore, cubic/quartic models are reasonable
# higher order are not justified
coef(summary(fit.5))

# ANOVA works with non-orthogonal polynomials as well
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)


# ANOVA alternative: polynomial degree CV (from Ch.5)
# Ex: Predict if someone makes >250/year
# Same as before, but create a response vector
# glm() w/ family="binomial"
fit=glm(I(wage>250)~poly(age,4),data=Wage,family="binomial")
# I() wrapper creates binary response variable on the fly
preds=predict(fit,newdata=list(age=age.grid),se=T)

pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

# directly compute probabilities using type="response"
preds=predict(fit,newdata=list(age=age.grid),type="response")

# plot right side of fig.7.1
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,2))
points(jitter(age),I((wage>250)/5),cex=0.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# jitter(): age values don't cover each other up (rug plot)
# To fit a step function, use cut():
table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))
# cut() picks cutpoints at 33.5,49,64.5 in age
# returns an ordered categorical variable

### 7.8.2: Splines
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="grey")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")
# R chooses knots at 33.75,42,51
# ns(): fits a natural spline
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

# specify knots using knots option
# use smooth.spline()
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=0.8)





