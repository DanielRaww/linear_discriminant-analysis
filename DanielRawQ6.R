rm(list=ls()) 
library(MASS)
library(mlbench)
library(caret)
library(dplyr)
library(ggplot2)

#Model 1
data(Glass)
set.seed(123)
str(Glass)
glass.lda<-lda(Type~., data=Glass)
glass.lda
divideData<- Glass$Type %>% createDataPartition(p=0.75, list = FALSE)
train<-Glass[divideData,]
test<-Glass[-divideData,]
preprocessing<-train %>% preProcess(method = c("center", "scale"))
#preprocessing isn't always necessary, but it never hurts
#allows you to compare on the same unit or level.  Like a z score
traintransformed<-preprocessing %>% predict(train)
testtransformed<-preprocessing %>% predict(test)
model<-lda(Type~., data = traintransformed)
model
plot(model)
ldaforgraph<-cbind(traintransformed, predict(model)$x)
ggplot(ldaforgraph, aes(LD1, LD2))+geom_point(aes(color=Type))
ggplot(ldaforgraph, aes(LD1, LD3))+geom_point(aes(color=Type))
ggplot(ldaforgraph, aes(LD1, LD4))+geom_point(aes(color=Type))
ggplot(ldaforgraph, aes(LD1, LD5))+geom_point(aes(color=Type))

#ggplot(ldaforgraph, aes(LD2, LD3))+geom_point(aes(color=Type))
#ggplot(ldaforgraph, aes(LD2, LD4))+geom_point(aes(color=Type))
#ggplot(ldaforgraph, aes(LD2, LD5))+geom_point(aes(color=Type))
#ggplot(ldaforgraph, aes(LD3, LD4))+geom_point(aes(color=Type))
#ggplot(ldaforgraph, aes(LD3, LD5))+geom_point(aes(color=Type))
#ggplot(ldaforgraph, aes(LD4, LD5))+geom_point(aes(color=Type))

prediction<- model %>% predict(testtransformed)
mean(prediction$class == testtransformed$Type) 
#accuracy rate is 57.69%
mean(prediction$class != testtransformed$Type) 
#error rate is 42.31%
table(prediction$class, testtransformed$Type)
#there are 22 misclassifications in the table

#Model2
summary(traintransformed)
str(traintransformed)
qdaModel<-qda(Type~., data=traintransformed)
#Cannot be calculated.
#The data set is incompatible to run a qda, there is not enough data to fit a quadratic model.
#In the traintransformed data, one of the groups is too small. In the type column, it also lacks Type 4.
