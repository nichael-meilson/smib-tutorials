### Linear Regression
# Set directory
setwd("~/Documents/School - Masters 2/Statistical Methods for Bioinformatics/smib-tutorials/1")

# Read data
kalama = read.table("kalama.txt", header=T)
kalama

# Descriptive statistics
# install.packages("pastecs")
library(pastecs)
options(digits=2)
descrip.kalama<-stat.desc(kalama[,c("age", "height")], basic=TRUE, desc=TRUE)
descrip.kalama

# Calculating covariances and correlation
cov.age.height<-cov(kalama$age,kalama$height)
corr.age.height<-cor(kalama$age,kalama$height)
cov.age.height
corr.age.height

# Testing if population correlation is zero
corr.age.height.test=cor.test(kalama$age, kalama$height, alternative="two.sided", method="pearson")
corr.age.height.test

## Fitting the model

res<-lm(height~age, data=kalama)
kalama.anova<-anova(res)
kalama.summary<-summary(res)
kalama.anova

kalama.summary







## Patient satisfaction
satisfaction=read.table("satisfaction.txt", header=T)
head(satisfaction,10)

## Exploring the data
cor(satisfaction)

options(digits=2)
descrip.satisfaction<-stat.desc(satisfaction, basic=TRUE, desc=TRUE)
descrip.satisfaction

plot(satisfaction)

## Fitting the model
# satis = B0 + B1*age + B2*severity + B3*anxiety
satisfaction.lm<-lm(satis~age+severity+anxiety, data=satisfaction)
satisfaction.summary<-summary(satisfaction.lm)
satisfaction.summary

# Rows in coefficient table:
# Comparing a model with every parameter vs. a model that 
# has every parameter except age
# Age is a significant predictor - null hypothesis rejected

# "Residual standard error" = sqrt(MSE) = 10
# "Multiple R-squared" = 68.2% of anxiety can be explained by this model


## Likelihood ratio test 
# null model vs. full model
# satis = B0 + error <-> satis = B0 + B1*age + B2*severity + B3*anxiety + error
satisfaction.lm.int<-lm(satis~1, data=satisfaction) #Null model
anova(satisfaction.lm.int, satisfaction.lm) #Null vs. full

# 1.5e-10 ***: H0: B1=B2=B3=0; all covariances 0
# F-statistic p-val: equal to anova table
satisfaction.summary


## Sequential building of the model
satisfaction.anova<-anova(satisfaction.lm)
satisfaction.anova

# Anxiety
# Same p-value as before (-1.9)^2=3.6 --> (t-val)^2 = F-val
satisfaction.summary


satisfaction.lm2<-lm(satis~age+anxiety+severity, data=satisfaction)
satisfaction.anova2<-anova(satisfaction.lm2)
satisfaction.anova2

# Severity
# Same p-val as previous summary table (-0.9)^2=0.81
satisfaction.summary


# What does anova do?
satisfaction.anova
# Sequentially building the model
# Compares a model that has nothing with a model that has a parameter
# Adds another parameter, working it's way up
# Start with nothing vs. B0, then B0 vs. B0+B1, etc.

## Final model
satisfaction.lm.final<-lm(satis~age+anxiety, data=satisfaction)
satisfaction.final.summary<-summary(satisfaction.lm.final)
satisfaction.final.summary


## Predicting a new observation
newdata = data.frame(age=43, anxiety=2.7)
pred.w.plim<-predict(satisfaction.lm.final, newdata, interval = "predict")
pred.w.clim<-predict(satisfaction.lm.final, newdata, interval = "confidence")
pred.w.plim # Value of the one individual
pred.w.clim # Estimate of the average of the sample


