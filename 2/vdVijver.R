library("glmnet")
library("pls")
library("randomForest")
library("tree")
library("gbm")
par(mfrow=c(1,1))
load(file="VIJVER.Rdata")
set.seed(1)
dim(data)
geneExpr=as.matrix(data[,-1])
meta= data[,1]
table(meta)
train=sample(1:nrow(geneExpr), nrow(geneExpr)*2/3)
test=(-train)
trainData=data[train,]
testData=data[test,]

correlationMatrix=(cor(geneExpr))
MT1=apply(correlationMatrix>0.9,2,sum)
length(which(MT1>1))/length(MT1)
#7% of genes have at least one gene for which correlation >0.9! 
MT2=apply(correlationMatrix>0.5,2,sum)
length(which(MT2>1))/length(MT2)
#79%

#illustrate some of the weaker correlations would already be identified as significant
plot(geneExpr[,3]~geneExpr[,4])
model=lm(geneExpr[,3]~geneExpr[,4])
summary(model)
abline(model)
cor(geneExpr[,4],geneExpr[,3])

index=3
plot(geneExpr[,index]~meta)
summary(as.factor(meta))
model=glm(meta~geneExpr[,index],family="binomial")
summary(model)

#do a lasso!
grid=10^seq(10,-2,length=100)

lasso.mod=glmnet(y=meta[train],x=(geneExpr[train,]),alpha=1,family="binomial")
plot(lasso.mod)
cv.out=cv.glmnet(geneExpr[train ,],meta[train],alpha=1,family="binomial")
plot(cv.out)
lasso.pred=predict(lasso.mod,s=cv.out$lambda.min,newx=geneExpr[test,],type="response")
plot(lasso.pred~meta[test])
pred=rep("DM",length(meta[test]))
pred[lasso.pred>0.5]="NODM"
table(meta[test],pred)
performanceLasso=length(which(pred==meta[test]))/length(meta[test])
performanceLasso #accuracy

vals=predict(lasso.mod,s=cv.out$lambda.min,type="coefficients")
selected=colnames(geneExpr)[vals@i]

#do a ridge!
ridge.mod=glmnet(y=meta[train],x=(geneExpr[train,]),alpha=0,family="binomial")
plot(ridge.mod)
ridge.cv=cv.glmnet(geneExpr[train ,],meta[train],alpha=0,family="binomial")
plot(ridge.cv)
ridge.pred=predict(ridge.mod,s=ridge.cv$lambda.min,newx=geneExpr[test,],type="response")
plot(ridge.pred~meta[test])
pred=rep("DM",length(meta[test]))
pred[ridge.pred>0.5]="NODM"
table(meta[test],pred)
performanceRidge=length(which(pred==meta[test]))/length(meta[test])
performanceRidge #accuracy

#do a pcr
metaRef= meta=="NODM"
pcr.mod <- pcr(metaRef ~ .,family=binomial(link=logit), data=data, subset=train, scale=TRUE,validation="CV")
summary(pcr.mod)
which.min(pcr.mod$CV)
validationplot(pcr.mod,val.type="RMSEP")#33 components
pcr.pred <- predict(pcr.mod,data[test,], ncomp=6,type="response")
pred=rep("DM",length(meta[test]))
hist(pcr.pred)#OOOOPS need pls-glm for logistic regression!
pred[pcr.pred>0.5]="NODM"
table(meta[test],pred)
performancePCR=length(which(pred==meta[test]))/length(meta[test])
performancePCR #accuracy

performanceLasso
performanceRidge
performancePCR

#do a tree
set.seed(1)
tr=tree(meta~.,data=trainData)
plot(tr)
text(tr,pretty=0,cex=0.65)
summary(tr)
cv.tr=cv.tree(tr,FUN=prune.misclass)
plot(cv.tr)
prune.tree=prune.misclass(tr,best=4)
plot(prune.tree)
text(prune.tree,pretty=0,cex=0.65)
summary(prune.tree)
predtree = predict(tr,newdata=testData,type="class")
table(meta[test],predtree) 
performanceTree1=length(which(predtree==meta[test]))/length(meta[test])
predtree2 = predict(prune.tree,newdata=testData,type="class")
table(meta[test],predtree2) 
performanceTree2=length(which(predtree2==meta[test]))/length(meta[test])
#this is a terrible result, and if you look at the trees it is obvious why:
#It presumes to reduce the complexity of the data to an extremely simple tree
#the summary reveals this solution works well on the training data. It is not generally true however
#a case of overlearning (high variance), and not capturing a generalizable relationship to the response in the data. 

#do a randomForest
set.seed(1)
rf=randomForest(y=meta[train],x=(geneExpr[train,]),mtry=(ncol(geneExpr)/3),importance=TRUE,ntree=1000,nodesize=20) 
predRF = predict(rf,newdata=geneExpr[test ,])
table(meta[test],predRF) 
performanceRF=length(which(predRF==meta[test]))/length(meta[test])
performanceRF
importance(rf)
varImpPlot (rf)

#do a bagged tree
bag=randomForest(y=meta[train],x=(geneExpr[train,]),mtry=ncol(geneExpr),importance=TRUE,ntree=1000) 
predbag = predict(bag,newdata=geneExpr[test ,])
table(meta[test],predbag) 
performanceBag=length(which(predbag==meta[test]))/length(meta[test])
performanceBag

varImpPlot(bag)

#do a boosted tree@
set.seed(1)
boost=gbm.fit(y=as.vector(meta[train]=="DM"),x=geneExpr[train,],distribution="bernoulli",n.trees=1000,interaction.depth=3,shrinkage=0.02)
#model.frame is very slow if there are many predictor variables, so the definition of x and y matrices in gbm.fit speeds up things considerably
best.iter <- gbm.perf(boost,method="OOB")#select number of trees with out of box estimates
predBO=predict(boost,newdata=geneExpr[test,],n.trees=best.iter,type="response")
pred=rep("DM",length(meta[test]))
pred[predBO<0.5]="NODM"
table(meta[test],pred)
performanceBO=length(which(pred==meta[test]))/length(meta[test])
performanceBO

