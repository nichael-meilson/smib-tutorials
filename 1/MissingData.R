## Set directory
setwd("~/Documents/School - Masters 2/Statistical Methods for Bioinformatics/smib-tutorials/1")

## Needed libraries
library(mice)
library(lattice)
library(VIM)
library(aod)
library(BaM)

## Reading data
titanic.missing <- read.table("titanic.txt", header=T, sep=",")
head(titanic.missing,10)

## Exploring missingness using the VIM library
titanic.missing.aggr=aggr(titanic.missing,numbers=T,prop=F,
                          ylab=c("Histogram of missing data","Pattern"))
titanic.missing.aggr

aggr(titanic.missing,combined=T, numbers=T, prop=T, cex.numbers=0.87,
     varheight=F)

## Amount of missingness in age for each survived group
barMiss(titanic.missing[,c("survived","age")])

## Amount of missingness in age for each sex group
barMiss(titanic.missing[,c("sex","age")])
histMiss(titanic.missing[,"age"])

## Fitting logistic regression model for the complete cases
titanic.logistic.omit<-glm(survived~pclass+sex+age, family = "binomial", data=titanic.missing)
summary(titanic.logistic.omit)

## Global effect of class
wald.test(b=coef(titanic.logistic.omit), Sigma=vcov(titanic.logistic.omit), Terms=2:3)

# Odds ratios
exp(cbind(OR=titanic.logistic.omit$coefficients, confint(titanic.logistic.omit)))

### Multiple imputation
# Patterns of missingness
pattern=md.pattern(titanic.missing)
pattern

pairs=md.pairs(titanic.missing)
pairs

# Imputing the missing values
imp <- mice(titanic.missing, m=10)
imp


# IPW
titanic.missing$r<-as.numeric(!is.na(titanic.missing$age))*as.numeric(!is.na(titanic.missing$sex))
head(titanic.missing,15)

## Fitting the logistic regression model to calculate the probabilities of being complete

titanic.ipw.glm<-glm(r~pclass+survived, data=titanic.missing, family = binomial)
summary(titanic.ipw.glm)

# Calculating weights: Inverse probabilities

titanic.missing$w<-1/fitted(titanic.ipw.glm)
head(titanic.missing,15)

titanic.results.ipw<-glm(survived~pclass+sex+age, data=titanic.missing, weights=titanic.missing$w, family=binomial)
summary(titanic.results.ipw)
