
library(dplyr)
library(ggplot2)
library(effects)
library(InformationValue)
rd=read.csv("C:/Users/HP PC/Documents/transfusion.data")

#Retrieve top few data
head(rd)

#information on the dataset
str(rd)

#rename column for brevity
names(rd)[5]<-"Target"

hd<-factor(rd$Target,levels = c(1,0),labels = c("possible","Not possible"))


#count number of 0 and 1 for Tarer incidencs to predict the donor wil donate blood next time or not
target_incidents<-table(rd$Target)

ggplot(rd,aes(x=Target,y=Time..months.))+geom_point()

#splitting into traing and test set
library(caTools)
set.seed(1234)
split<-sample.split(rd$Target,SplitRatio = 1/4)
training_set<-subset(rd,split==TRUE)
test_set<-subset(rd,split==FALSE)

head(training_set,n=2)

#check the variance
var(training_set)
summary(training_set)


#correct high variance using log normalizetion
x_train_normed<-training_set
x_test_normed<-test_set

col_to_noramlize<-x_train_normed$Monetary..c.c..blood.

log10(col_to_noramlize)
summary(x_train_normed)
var(x_train_normed)


#fitting logistic reggression
lgt_reg<-glm(formula=Target~Recency..months.+Frequency..times.+Time..months.,family = binomial ,data = x_train_normed)
summary(lgt_reg)

#predict test set result
y_pred<-predict(lgt_reg,type = 'response' ,newdata =x_test_normed)
pred<-ifelse(y_pred>0.5,1,0)

table(test_set$Target,pred)
table(pred)


#visualizing through ROC curve

plot(allEffects(lgt_reg))

plotROC(test_set$Target,pred)


