#SINGLE SVM
library(colorspace)
library(rpart)
library(e1071)
library(MASS)


dataset <- read.csv("E:/thesis/SVM/hepatitis data csv.csv",header=T,sep=";")
attach(dataset)
index <- 1:nrow(dataset)
testindex <- sample(index, trunc(length(index)*30/100))
testset <- dataset[testindex,]
trainset <- dataset[-testindex,]
trainindex <- sample(index, trunc(length(index)*70/100))
tuned <- tune.svm(class~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))
cc <- as.numeric(tuned$best.parameters[2])
gg <- as.numeric(tuned$best.parameters[1])

modelolin <- svm(class ~ ., trainset, type = "C-classification", cost = cc, gamma = gg, kernel = "linear")
modelopoly <- svm(class ~ ., trainset, type = "C-classification", cost = cc, gamma = gg, kernel = "polynomial")
modelorad <- svm(class ~ ., trainset, type = "C-classification", cost = cc,gamma = gg, kernel = "radial")
modelotan <- svm(class ~ ., trainset, type = "C-classification", cost = cc,gamma = gg, kernel = "sigmoid")

prediction1<- predict(modelolin,newdata=testset, decision.values=F)
prediction2<- predict(modelopoly,newdata=testset, decision.values=F)
prediction3<- predict(modelorad,newdata=testset, decision.values=F)
prediction4<- predict(modelotan,newdata=testset, decision.values=F)

tab1 <- table(pred = prediction1,testset$class)
tab2 <- table(pred = prediction2,testset$class)
tab3 <- table(pred = prediction3,testset$class)
tab4 <- table(pred = prediction4,testset$class)

ecr1<-(tab1[2,1]+tab1[1,2])/sum(tab1)
ecr2<-(tab2[2,1]+tab2[1,2])/sum(tab2)
ecr3<-(tab3[2,1]+tab3[1,2])/sum(tab3)
ecr4<-(tab4[2,1]+tab4[1,2])/sum(tab4)

classAgreement(tab)


#Bagging EnSVM
library(colorspace)
library(rpart)
library(e1071)
library(MASS)

dataset <- read.csv("E:/thesis/SVM/hepatitis data csv.csv",header=T,sep=";")
 attach(dataset)
 m<-10
 k<-10
index <- 1:nrow(dataset)
 testindex <- sample(index, trunc(length(index)*30/100))
 testset <- dataset[testindex,]
 trainset <- dataset[-testindex,]
trainindex<- sample(index, trunc(length(index)*70/100))

 es<-numeric(m)
 eb<-matrix(nrow=m,ncol=k)

 eslin<-numeric(m)
 espoly<-numeric(m)
 esrad<-numeric(m)
 estan<-numeric(m)

eblin<-matrix(nrow=m,ncol=k)
ebpoly<-matrix(nrow=m,ncol=k)
ebrad<-matrix(nrow=m,ncol=k)
ebtan<-matrix(nrow=m,ncol=k)

set.seed(123)
for (i in 1:m){
     index <- 1:nrow(dataset)
     testindex <- sample(index, trunc(length(index)*30/100))
     testset <- dataset[testindex,]
     trainset <- dataset[-testindex,]
     tuned <- tune.svm(class~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))
     cc <- as.numeric(tuned$best.parameters[2])
     gg <- as.numeric(tuned$best.parameters[1])

     modellin <- svm(class ~ ., trainset, type = "C-classification", cost = cc, gamma = gg, kernel = "linear")
     modelpoly <- svm(class ~ ., trainset, type = "C-classification", cost = cc, gamma = gg, kernel = "polynomial")
     modelrad <- svm(class ~ ., trainset, type = "C-classification", cost = cc,gamma = gg, kernel = "radial")
     modeltan <- svm(class ~ ., trainset, type = "C-classification", cost = cc,gamma = gg, kernel = "sigmoid")

     prediction1<- predict(modellin,newdata=testset, decision.values=F)
     prediction2<- predict(modelpoly,newdata=testset, decision.values=F)
     prediction3<- predict(modelrad,newdata=testset, decision.values=F)
     prediction4<- predict(modeltan,newdata=testset, decision.values=F)

     no1<-sum(prediction1==0)
     yes1<-sum(prediction1==1)
     vote1<-c()
     if(yes1>no1) vote1<-1 else vote1<-0

     no2<-sum(prediction2==0)
     yes2<-sum(prediction2==1)
     vote2<-c()
     if(yes2>no2) vote2<-1 else vote2<-0

     no3<-sum(prediction3==0)
     yes3<-sum(prediction3==1)
     vote3<-c()
     if(yes3>no3) vote3<-1 else vote3<-0

     no4<-sum(prediction4==0)
     yes4<-sum(prediction4==1)
     vote4<-c()
     if(yes4>no4) vote4<-1 else vote4<-0

     tab1 <- table(pred = prediction1,testset$class)
     tab2 <- table(pred = prediction2,testset$class)
     tab3 <- table(pred = prediction3,testset$class)
     tab4 <- table(pred = prediction4,testset$class)

     ecr1<-(tab1[2,1]+tab1[1,2])/sum(tab1)
     ecr2<-(tab2[2,1]+tab2[1,2])/sum(tab2)
     ecr3<-(tab3[2,1]+tab3[1,2])/sum(tab3)
     ecr4<-(tab4[2,1]+tab4[1,2])/sum(tab4)

     eslin[i]<-ecr1
     espoly[i]<-ecr2
     esrad[i]<-ecr3
     estan[i]<-ecr4

     set.seed(123)
     for (j in 1:k) {
          boot<-sample(trainindex,replace=TRUE)
          sboot<-trainset[boot,]
          tuned <- tune.svm(class~., data = sboot, gamma = 10^(-6:-1), cost = 10^(-1:1))
          cc <- as.numeric(tuned$best.parameters[2])
          gg <- as.numeric(tuned$best.parameters[1])

          modellin1 <- svm(class ~ ., sboot, type = "C-classification", cost = cc, gamma = gg, kernel = "linear")
          modelpoly2 <- svm(class ~ ., sboot, type = "C-classification", cost = cc, gamma = gg, kernel = "polynomial")
          modelrad3 <- svm(class ~ ., sboot, type = "C-classification", cost = cc,gamma = gg, kernel = "radial")
          modeltan4 <- svm(class ~ ., sboot, type = "C-classification", cost = cc,gamma = gg, kernel = "sigmoid")

          predictions1<- predict(modellin1,newdata=testset, decision.values=F)
          predictions2<- predict(modelpoly2,newdata=testset, decision.values=F)
          predictions3<- predict(modelrad3,newdata=testset, decision.values=F)
          predictions4<- predict(modeltan4,newdata=testset, decision.values=F)

          noboot1<-sum(predictions1==0)
          yesboot1<-sum(predictions1==1)
          voteboot1<-c()
          if(yesboot1>noboot1) voteboot1<-1 else voteboot1<-0

          noboot2<-sum(predictions2==0)
          yesboot2<-sum(predictions2==1)
          voteboot2<-c()
          if(yesboot2>noboot2) voteboot2<-1 else voteboot2<-0

          noboot3<-sum(predictions3==0)
          yesboot3<-sum(predictions3==1)
          voteboot3<-c()
          if(yesboot3>noboot3) voteboot3<-1 else voteboot3<-0

          noboot4<-sum(predictions4==0)
          yesboot4<-sum(predictions4==1)
          voteboot4<-c()
          if(yesboot4>noboot4) voteboot4<-1 else voteboot4<-0

          tabs1 <- table(pred = predictions1,testset$class)
          tabs2 <- table(pred = predictions2,testset$class)
          tabs3 <- table(pred = predictions3,testset$class)
          tabs4 <- table(pred = predictions4,testset$class)

          ecrbootmodellin<-(tabs1[2,1]+tabs1[1,2])/sum(tabs1)
          ecrbootmodelpoly<-(tabs2[2,1]+tabs2[1,2])/sum(tabs2)
          ecrbootmodelrad<-(tabs3[2,1]+tabs3[1,2])/sum(tabs3)
          ecrbootmodeltan<-(tabs4[2,1]+tabs4[1,2])/sum(tabs4)

          eblin[i,j]<-ecrbootmodellin
          ebpoly[i,j]<-ecrbootmodelpoly
          ebrad[i,j]<-ecrbootmodelrad
          ebtan[i,j]<-ecrbootmodeltan
     }
}