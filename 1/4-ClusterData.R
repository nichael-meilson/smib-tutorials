## Set directory
setwd("~/Documents/School - Masters 2/Statistical Methods for Bioinformatics/smib-tutorials/1")

## Reading the data
ratpup <- read.table("rat_pup.dat", h = T)
ratpup$sex1[ratpup$sex == "Female"] <- 1
ratpup$sex1[ratpup$sex == "Male"] <- 0
attach(ratpup)

## Table describing the data
library(dplyr)
g <- function(x)c(N=length(x),Mean=mean(x,na.rm=TRUE),
                  SD=sd(x,na.rm=TRUE), Min=min(x,na.rm=TRUE),Max=max(x,na.rm=TRUE))
summarize(weight,by=llist(treatment,sex),g)

library(lattice)
library(grid)

par(mfrow=c(1,1))
bwplot(weight ~ sex|treatment, data=ratpup,aspect = 2,
      ylab="Birth Weights", xlab="SEX",
      main = "Boxplots of birth weights for levels of treatment by sex")


## Fitting the homocedastic model
library(nlme)
meanfull.hom <- lme(weight ~ treatment + sex1 + litsize + treatment:sex1,
                                random = ~1 | litterid, ratpup, method = "REML")
summary(meanfull.hom)
anova(meanfull.hom)
# Cannot compare summary and anova because they are fit with REML
# Mean structures cannot compare
# Measuring model with different fixed effects

## Display the random effects (EBLUPs) from the model.
random.effects(meanfull.hom)

## Fitting a heterocedastic model
meanfull.het <- lme(weight ~ treatment + sex1 + litsize + treatment:sex1,
                  random = ~1 | litterid, ratpup, method = "REML",
                  weights = varIdent(form = ~1 | treatment))
summary(meanfull.het)

## Heterocedastic versus homocedastic model
anova(meanfull.hom,meanfull.het)

## High-low dose: Equal residual variance

ratpup$trtgrp[treatment=="Control"] <- 1
ratpup$trtgrp[treatment == "Low" | treatment == "High"] <- 2

meanfull.hilo <- lme(weight ~ treatment + sex1 + litsize + treatment:sex1,
                     random = ~1 | litterid, ratpup, method = "REML",
                     weights = varIdent(form = ~1 | trtgrp))

summary(meanfull.hilo)
anova(meanfull.hilo)


## High-low dose: Equal residual variance
ratpup$trtgrp[treatment=="Control"] <- 1
ratpup$trtgrp[treatment == "Low" | treatment == "High"] <- 2
meanfull.hilo <- lme(weight ~ treatment + sex1 + litsize + treatment:sex1,
                     random = ~1 | litterid, ratpup, method = "REML",
                     weights = varIdent(form = ~1 | trtgrp))
summary(meanfull.hilo)
anova(meanfull.hilo)

anova(meanfull.het,meanfull.hilo)

## Is there a litter effect?

meanfull.hilo.nolitter<-gls(weight~treatment+sex1+litsize+treatment:sex1, data=ratpup, 
                            weights=varIdent(form=~1|trtgrp))
summary(meanfull.hilo.nolitter)

anova(meanfull.hilo.nolitter, meanfull.hilo)
# p-val < 0.05; accept null hypothesis, no litter effect

## Fitting the final model using ML
meanful.hilo.ml<-lme(weight~treatment+sex1+litsize+treatment:sex1,random=~1|litterid, ratpup, method="ML",
                     weights=varIdent(form=~1|trtgrp))
summary(meanful.hilo.ml)
anova(meanful.hilo.ml)

