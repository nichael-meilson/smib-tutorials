# SMiB - Lab 6.2
### 6.6 - RIDGE REGRESSION/LASSO
library(ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters)
library(glmnet)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

### 6.6.1 - RIDGE REGRESSION
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]

ridge.mod$lambda[60]
coef(ridge.mod)[,60]

predict(ridge.mod,a=50,type="coefficients")[1:20,]

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lamda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)
mean((ridge.pred-y.test)^2)
