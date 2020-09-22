rm(list=ls())
#-----------------------------------------------------------------------------loading datasets
setwd("D:/Coursera/sparks/task4")
iris<-read.csv("Iris.csv")
#-----------------------------------------------------------------------------splitting dataset
set.seed(42)
rows<-sample(nrow(iris))
iris<-iris[rows,]
split<-sample.split(iris,SplitRatio = 0.7)
train<-subset(iris,split="TRUE")
test<-subset(iris,split="FALSE")
#-----------------------------------------------------------------------------modelling
library(caTools)
library(rpart)
library(rpart.plot)
decisiontree_model<-rpart(Species~PetalLengthCm+PetalWidthCm+SepalLengthCm+SepalWidthCm,data=train,method="class")
decisiontree_model
rpart.plot(decisiontree_model)
#-----------------------------------------------------------------------------predicting
iris$Species_Predicted<-predict(decisiontree_model,newdata = test,type="class")
table(iris$Species,iris$Species_Predicted)
library(caret)
confusionMatrix(table(iris$Species,iris$Species_Predicted))
#-----------------------------------------------------------------------------Tree pruning
#the printcp and plotcp functions provide the cross validation error for eah split
#the one with the least cross validation error is the optimal value of CP given
printcp(decisiontree_model)
plotcp(decisiontree_model)
#minimum error occurs when tree size is 3
#value of cp for which cross validation error is minimum
min(decisiontree_model$cptable[,"xerror"])
which.min(decisiontree_model$cptable[,"xerror"])
cpmin<-decisiontree_model$cptable[3,"CP"]

#pruning the tree
decision_tree_prune= prune(decisiontree_model,cp=cpmin)
rpart.plot(decision_tree_prune)
