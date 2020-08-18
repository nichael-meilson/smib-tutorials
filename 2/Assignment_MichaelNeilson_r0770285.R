### Statistical Methods in Bioinformatics, Part 2
# Assignment
# Michael Neilson
# r0770285


# For this assignment you will use the dataset “prostate” (prostate2.Rdata). The dataset contains data
# about prostate cancer patients with information on the size of the prostate, the age of the patient, a
# blood marker (lpsa) and so on. The response variable is a score (Cscore) on the progression of the
# cancer after detailed study of the tumor pathology.
setwd("~/Documents/School - Masters 2/Statistical Methods for Bioinformatics/Part 2")
load("prostate2.Rdata")
head(prostate)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Study and describe the predictor variables. Do you see any issues that are relevant for
# making predictions?

# Look at scatterplot for variable associations
par(mfrow=c(4,2))
for (col in 1:ncol(prostate)) {
  plot(prostate[,col],main=colnames(prostate[col]),xlab="Index",ylab="Value")
}

# Look at histograms for distribution
par(mfrow=c(4,2))
for (col in 1:ncol(prostate)) {
  hist(as.numeric(prostate[,col]),main=colnames(prostate[col]),xlab="Value")
}

# Look at the correlation plot for collinearity
par(mfrow=c(1,1))
library(corrplot)
correlations=cor(prostate)
corrplot(correlations,method="number")

# Based on the scatterplots of the data, it looks like the only variable that isn't a continuous variable is "svi", which is binary.
# The data looks to be sorted by lpsa, which may be correlated to the response variable CScore.
# 
# Based on the histograms for each predictor variable and the response variable,
# it does not look like there is much collinearity between each of the predictors or the response variable
# 
# By evaluating the correlation plot, this appears to be true as there are no strong correlations, thus no multicollinearity.
# The strongest correlations look to be between CScore & lpsa, lcavol & lcp, and lcavol & lpsa - but all weak (R = <0.75)
# The data isn't too highly dimensional so we should be able to fit a good model without subsetting or taking PCs

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2. Make an appropriate LASSO model, with the appropriate link and error function, and
# evaluate the prediction performance.
library(glmnet)
prostate$svi = as.factor(prostate$svi)

# Make sure there are no nulls
sum(is.na(prostate)) # == 0

# Set test/train groups
set.seed(1)
x=model.matrix(Cscore~.,prostate)[,-1]
y=prostate$Cscore

train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# Set up Lasso model
grid=10^seq(10,-2,length=100)
lasso.model=glmnet(x[train,],y[train],alpha=1,lambda=grid)
par(mfrow=c(1,1))
plot(lasso.model)
# Some coefficients will be equal to 0.

# Cross validation:
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
lambda.min=cv.out$lambda.min
lasso.prediction=predict(lasso.model,s=lambda.min,newx=x[test,])
lasso.mse=mean((lasso.prediction-y.test)^2) # MSE
lasso.mse
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Look at the coefficient for “lcavol” in your LASSO model. Does this coefficient correspond
# to how well it can predict Cscore? Explain your observation.

out=glmnet(x,y,alpha=1,lambda = lambda.min)
predict(out,type="coefficients",s=lambda.min)

# The coefficient here is the output of the OLS coefficients "shrunk" by lambda, as in penalizing large predictors
# For "lcavol", this coefficient is relatively small, so it explains less of the variance in the model.
# It does not directly correspond to how well it can predict the Cscore, but how much the least squares coefficients need to be tuned
# to minimize the MSE of the model.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Fit a model with *appropriate* non-linear effects. Report a comparison of performance to
# LASSO and explain what you find. 

library(gam)
GAMselection <- function(data, degree){
  response='Cscore~'
  sep=''
  p=ncol(data)
  for (k in 2:p){
    vars=names(data)[k]
    if (is.factor(data[,k])){
      newform = vars
    } else {
      newform=paste('s(',vars,', ',deg[k],')',sep='') 
    }
    response = paste(response, newform, sep=sep)
    sep='+'
  }
  return(response)
}

set.seed(1)
nvars = ncol(prostate)
total.aic=rep(NA, nvars)
total.model=rep('', nvars)
deg=rep(4,nvars)
training.data=prostate[train,]
for (i in  nvars:2) {
  model = GAMselection(training.data, deg)
  cat('i = ', i, ' : ', model, '\n')
  regr = gam(as.formula(model), data=training.data)
  model.aic = AIC(regr)
  total.model[i] = model
  total.aic[i] = model.aic
  if (i > 1) {
    new.aic = rep(NA, i)
    for (j in 2:i) {
      deg.next = deg
      deg.next[j] = 0
      model.next = GAMselection(training.data, deg.next)
      cat('evaluate removal of var j = ', j, ' : ', model.next, '\n')
      model.reg = gam(as.formula(model.next), data=training.data)
      new.aic[j] = AIC(model.reg)
    }
    remove.var = which.min(new.aic)
    training.data = training.data[,-remove.var]
    deg = deg[-remove.var]
  }
}

par(mfrow=c(1,1))
plot(total.aic, main="AIC per Number of Predictors (DoF = 4)", xlab="Predictors", ylab="AIC")
total.model[which.min(total.aic)]
selected.model=total.model[which.min(total.aic)]
gam.model=gam(as.formula(selected.model), data=prostate[train,])
summary(gam.model)

# Refine with linear effects, if possible
gam.model2=gam(Cscore~s(lcavol, 4)+s(lweight, 4)+s(age, 4)+lbph+svi+s(lcp,4)+s(lpsa, 4),data=prostate[train,])
summary(gam.model2)
gam.model3=gam(Cscore~s(lcavol, 4)+s(lweight, 4)+age+lbph+svi+s(lcp,4)+s(lpsa,4),data=prostate[train,])
summary(gam.model3)
gam.model4=gam(Cscore~lcavol+s(lweight, 4)+age+lbph+svi+s(lcp,4)+s(lpsa,4),data=prostate[train,])
summary(gam.model4)
# Try to remove age and lbph like lasso did
gam.model5=gam(Cscore~lcavol+s(lweight, 4)+svi+s(lcp,4)+s(lpsa,4),data=prostate[train,])
summary(gam.model5)

AIC(gam.model)
AIC(gam.model2)
AIC(gam.model3)
AIC(gam.model4) # <-- this one is lowest
AIC(gam.model5)

### Compare
# MSE
gam.ypred=predict(gam.model4,newdata=prostate[test,], type="response")
gam.mse=mean((gam.ypred-y.test)^2) # MSE
gam.mse #679.405
lasso.mse #725.9418

# Plot
par(mfrow=c(1,1))
plot(prostate[test,]$Cscore,ylim=c(-100,200),ylab="CScore",main="Fitting Test Data - Predicted vs. Actual")
points(lasso.prediction,col=2,pch=2)
points(gam.ypred,col=3,pch=3)  
legend(1,200,legend=c("Test Data","Lasso Prediction","GAM Prediction"),col=c(1,2,3), pch=c(1,2,3))
