set.seed(40)
x1=rnorm(100)
x2=rnorm(100)*1.5+2-x1/2
y=1+x1-0.2*x2+0.01*x1**3+rnorm(100)
par(mfrow=c(1,1))
plot(y,x1)
plot(y,x2)
plot(x2,x1)

b0=rep(0,1000)
b1=rep(0,1000)
b2=rep(0,1000)
beta1=10
for (i in 1:1000){
  a =y - beta1 * x1
  beta2 = lm ( a~x2 ) $coef [2]
  a =y - beta2 * x2
  lmT = lm ( a~x1 )
  beta1=lmT$coef[2]
  b0[i]=lmT$coef[1]
  b1[i]=beta1
  b2[i]=beta2
}
par(mfrow=c(1,1))
plot(1:10,b0[1:10],col=1,type="l",ylim=range(b0,b1,b2))#it is basically stable after 10 iterations
lines(1:10,b1[1:10],col=2)
lines(1:10,b2[1:10],col=3)

summary(lm(y~x1+x2+x1*x2))
