## Set directory
setwd("~/Documents/School - Masters 2/Statistical Methods for Bioinformatics/smib-tutorials/1")

## Reading early.int data
early.int1 <- read.table("earlyint.txt", header=T, sep=",")

# Attach data to search path
attach(early.int1)

# Spaghetti plot
n=length(unique(id))
interaction.plot(age,id,cog,xlab = "Age (years)", ylab = "IQ", legend = F)

## Description
# Mean
early.mean=tapply(cog,list(age,program),mean)
# Std Dev
early.sd=tapply(cog,list(age,program),sd)
# Variance
early.var=tapply(cog,list(age,program),var)
# Frequency
early.n=table(age,program)

## Boxplots
boxplot(cog~age,xlab = "Age", ylab = "IQ")
# per program
boxplot(cog[program==0]~age[program==0],main="No intervention",
        xlab = "Age", ylab = "IQ")
boxplot(cog[program==1]~age[program==1],main="No intervention",
        xlab = "Age", ylab = "IQ")


## General function to plot error bars
errbar=function(x,y,height,width,lty=1,col="black") {
  arrows(x,y,x,y+height,angle=90,length=width,lty=lty,col=col)
  arrows(x,y,x,y-height,angle=90,length=width,lty=lty,col=col)
}

# Plotting evolutions
plot(age[id==1], early.mean[,1],type="b",xlim=c(1,2),ylim = c(40,160),xlab = "Age",ylab = "IQ",axes = F,
     main="Mean evolution (w/ 1 SE intervals)")
axis(side=1,at=c(1,1.5,2),labels = c(1,1.5,2))
axis(side=2,at=seq(40,160,20))

box()
points(age[id==1],early.mean[,2],type="b",col="red")
errbar(age[id==1]-.005,early.mean[,1],early.sd[,1],.1)
errbar(age[id==1]+.005,early.mean[,2],early.sd[,2],.1,col="red")

## Reshaping data into wide form
early.int2<-reshape(early.int1,timevar = "age",idvar = c("id","program"),direction="wide")
early.int2

# Correlations between IQ scores at different ages
cor(early.int2[,3:5])


## Creating the time variable
early.int1$age0<-early.int1$age-1

# Displaying linear regression per person
# Trellis Graph
cf<-sapply(early.int1$id,function(x)
  coef(lm(cog~age0,data = subset(early.int1,id=x))))
Sx<-reorder(early.int1$id, cf[1,])
library(lattice)
xyplot(cog~age0|Sx,groups=program,data=early.int1,
       type=c('p','r'),auto.key=T,aspect="xy",
       par.settings=list(axis.text=list(cex=0.6),
                         fontsize=list(text=8,points=10)),
       scales=list(x=list(at=c("0","0.5","1")))
       )

## Linear regression per participant of cog on age

# coefficients
lin.reg.coef <- by(early.int1, early.int1$id,
                   function(data) coef(lm(cog ~ age0, data=data)))
lin.reg.coef1 <- unlist(lin.reg.coef)
names(lin.reg.coef1) <- NULL
lin.reg.coef2=matrix(lin.reg.coef1,length(lin.reg.coef1)/2,2,byrow = TRUE)

# R-squared
lin.reg.r.squared <- by(early.int1, early.int1$id,
                        function(data) summary(lm(cog ~ age, data=data))$r.squared )
lin.reg.r.squared1<- as.vector(unlist(lin.reg.r.squared))

# Histograms
par(mfrow=c(3,1))
hist(lin.reg.coef2[,1],xlab="Intercept",col="lightblue",main="Histogram of individual intercepts")
hist(lin.reg.coef2[,2],xlab="Slope",col="lightblue",main="Histogram of individual slopes")
hist(lin.reg.r.squared1,xlab="R squared",col="lightblue",main="Histogram of individual R squared")


## Plotting individual regression lines per group
reg.coef=cbind(lin.reg.coef2, early.int1[early.int1$age==1,]$program)

mean.int<-tapply(reg.coef[,1],reg.coef[,3],mean)
mean.slope<-tapply(reg.coef[,2],reg.coef[,3],mean)

par(mfrow=c(1,2))
plot(age,cog,type="n",xlim=c(1,2),ylim=c(40,160),main="No intervention",
     xlab="Age-1 (in years)",ylab="IQ",axes=F)
axis(side=1,at=c(1,1.5,2),labels=c(1,1.5,2))
axis(side=2,at=seq(40,160,20))
box()
for (i in 1:103)
  if (reg.coef[i,3]==0)
    curve(cbind(1,x)%*%reg.coef[i,1:2],add=T,col="gray")
curve(cbind(1,x)%*%c(mean.int[1],mean.slope[1]),add=T,lwd=2)

plot(age,cog,type="n",xlim=c(1,2),ylim=c(40,160),main="Intervention",
     xlab="Age-1 (in years)",ylab="IQ",axes=F)
axis(side=1,at=c(1,1.5,2),labels=c(1,1.5,2))
axis(side=2,at=seq(40,160,20))
box()
for (i in 1:103)
  if (reg.coef[i,3]==1)
    curve(cbind(1,x)%*%reg.coef[i,1:2],add=T,col="gray")
curve(cbind(1,x)%*%c(mean.int[2],mean.slope[2]),add=T,lwd=2)


library(lme4)
library(arm)
library(car)
library(pbkrtest)

# Creating the time variable
early.int1$age0<-early.int1$age-1

# Fitting the model with ML
# Slide 216
early.lmer1<-lmer(cog~1+age0*program+(1+age0|id),REML=F, data=early.int1)
summary(early.lmer1)

display(early.lmer1)
anova(early.lmer1)


## Estimating fixed effects via bootstrap
# Slide 230
fixed.boot=bootMer(early.lmer1, fixef, use.u = TRUE, nsim = 250)
fixed.boot
summary(fixed.boot)

## Calculating confidence intervals for the fixed effects via Wald, bootstrap and profile likelihood
confint(early.lmer1,par=5:8,method="Wald",oldNames = FALSE) # Only for fixed effects vc will return NA

confint(early.lmer1,method="boot",boot.type ="perc",oldNames = FALSE,nsim=500)

confint(early.lmer1, level = 0.95,method="profile",oldNames = FALSE)

# KR-approximated degree of freedom
early.lmer1.df.KR <- get_ddf_Lb(early.lmer1, fixef(early.lmer1))





