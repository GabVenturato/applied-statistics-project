library(rminer)

library(ggplot2)
library(kknn)
library(ggpubr)
library(corrplot)

lifeexp.df = read.csv("Life Expectancy Data.csv")

# dataset description

str(lifeexp.df)
summary(lifeexp.df) # here we can see NAs

# DATA PREPARAION

## Country Factor to Numerical
lifeexp.df$Country = as.numeric(lifeexp.df$Country)

## IMPUTATION
# save column with missing values indexes
nacol = NULL
for (i in 1:ncol(lifeexp.df)) {
  if ( any(is.na(lifeexp.df[,i])) ) {
    nacol = c(nacol,i)
  }
}
    
# 1st method: case deletion
lifeexp.na.del = na.omit(lifeexp.df)
summary(lifeexp.na.del)

# 2nd method: imputation by mode
lifeexp.imp.mode = lifeexp.df
for (i in nacol) {
  lifeexp.imp.mode = imputation("value", lifeexp.imp.mode, i, Value=which.max(table(na.omit(lifeexp.df[,i]))))
}
summary(lifeexp.imp.mode)

# 3rd mode: imputation by hotdeck
lifeexp.imp.hotdeck = lifeexp.df
for (i in nacol) {
  lifeexp.imp.hotdeck = imputation("hotdeck", lifeexp.imp.hotdeck, i)
}
summary(lifeexp.imp.hotdeck)

# imputation comparison
plots = list()
j     = 1
for (i in nacol[1:4]) {
    meth1=data.frame(v=lifeexp.na.del[[i]])
    meth2=data.frame(v=lifeexp.imp.mode[[i]])
    meth3=data.frame(v=lifeexp.imp.hotdeck[[i]])
    meth1$Method="NAs deleted"
    meth2$Method="Mode"
    meth3$Method="Hotdeck"
    all = rbind(meth1,meth2,meth3)
    plots[[j]] = ggplot(all,aes(v,fill=Method))+geom_density(alpha = 0.4)+xlab(colnames(lifeexp.df)[i])
    j = j+1
}
ggarrange(plotlist = plots, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

# we keep hotdeck version
lifeexp = lifeexp.imp.hotdeck

## EDA
correlation = cor(within(lifeexp, rm("Status")))
corrplot(correlation, type="upper", method="circle")

# MODEL
inputs  = c(1:2,4:ncol(lifeexp))
dvar    = 3

# Holdout - Random Forest 
H = holdout(lifeexp$Life.expectancy, ratio=2/3, seed=12345)
summary(H)
model1 = fit( Life.expectancy~., lifeexp[H$tr,], model="randomForest")

# Crossvalidation - Random Forest
model2 = crossvaldata(Life.expectancy~., lifeexp, fit, predict, ngroup=10, seed=123, model="randomForest", task="reg")

# EVALUATION
# Holdout
# get predictions on test set (new data)
pred1 = predict(model1, lifeexp[H$ts,])
target1 = lifeexp[H$ts,]$Life.expectancy
mgraph(target1, pred1, graph="RSC", Grid=10, main="Random Forest - Holdout 1/3")
mmetric(target1, pred1, metric="ALL")

# 10-fold cross-validation
pred2 = model2$cv.fit # k-fold predictions on full dataset
mgraph(lifeexp$Life.expectancy, pred2, graph="RSC", Grid=10, main="Random Forest - 10-fold Cross Validation")
mmetric(lifeexp$Life.expectancy, pred2, metric="ALL")


# mining for randomForest, external 3-fold, 20 Runs (=60 fitted models)
model3.mining = mining(Life.expectancy~., lifeexp, model="randomForest", method=c("kfold",3,42), Runs=20)
m=mmetric(model3.mining, metric=c("MAE","RMSE","R2")) # 2 metrics:
print(m) # show metrics for each run

# mining for standard multiple linear regression
model4.mining = mining(Life.expectancy~., lifeexp, model="mr", method=c("kfold",3,42), Runs=20)

L=vector("list",2) # list of minings
L[[1]]=model3.mining
L[[2]]=model4.mining
mgraph(L, graph="REC", leg=c("randomForest","mr"), main="REC curve", xval=10)