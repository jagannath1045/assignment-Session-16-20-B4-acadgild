#session16_assisgnment_16 task 1 

#1. Use the below given data set
#Data Set
library(readr)
library(data.table)
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
library(caret)
library(glmnet)
install.packages("mlbench")
library(mlbench)
getwd()
data1<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_1.csv")
data2<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_2.csv")
data3<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_3.csv")
data4<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_4.csv")
data5<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_5.csv")
train_set<-rbind(data1,data2,data3,data4,data5)
train_set1<-na.omit(train_set)
train_set1
#data(train_set1, package="mlbench")


data6<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_1.csv")
data7<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_2.csv")
data8<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_3.csv")
data9<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_4.csv")
data10<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_5.csv")
data11<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_6.csv")

test_set<-rbind(data6,data7,data8,data9,data10)
fbmsg<-rbind(test_set,data6)

colSums(is.na(fbmsg))


#2. Perform the below given activities:
# a. Predict the no of comments in next H hrs

x<-as.matrix(fbmsg[,1:53])
y<-as.matrix(fbmsg[,54])

########Note:-
#1. Use LASSO, Elastic Net and Ridge and other regression techniques that are covered in the module

library(glmnet)
fit_ridge<-glmnet(x,y, family = "gaussian", alpha = 0, lambda = 0.001)
lasso
summary(fit_ridge)
predictions1<-predict(lasso, x, type = "link")
mse1<-mean((y-predictions1)^2)
mse1

library(glmnet)
fit_lasso<-glmnet(x,y, family = "gaussian", alpha = 1, lambda = 0.001)
fit_lasso
summary(fit_lasso)
predictions2<-predict(fit_lasso, x, type = "link")
mse2<-mean((y-predictions2)^2)
mse2


fit_elnet<-glmnet(x,y, family = "gaussian", alpha = 0.5, lambda = 0.001)
fit_elnet
summary(fit_elnet)
predictions3<-predict(fit_elnet, x, type = "link")
mse3<-mean((y-predictions3)^2)
mse3

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x, y, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}


# Plot solution paths:
par(mfrow=c(3,2))




library(earth)
model1<-earth(V54~., data = fbmsg)
model1

summary(model1)
evimp(model1)
predictions4<-predict(model1, fbmsg)

mse4<-mean((fbmsg$V54-predictions4)^2)
mse4

### step wise regression
base<-lm(V54~., fbmsg)
base

summary(base)

fitt<-step(base)
summary(fitt)
prediction5<-predict(fitt, fbmsg)

mse5<-mean((fbmsg$V54-prediction5)^2)
mse5



library(MASS)

fit_rgm<-rlm(V54~., data=fbmsg)
fit_rgm

#2. Report the training accuracy and test accuracy

#Lasso model accuracy prediction
mse<-mean((y-predictions2)^2)
mse

# Ridge model accuracy prediction
mse2<-mean((fbmsg$V54-predictions3)^2)
mse2

# stepwise model accuracy 
mse5<-mean((fbmsg$V54-prediction5)^2)
mse5

#3. compare with linear models and report the accuracy

library(lars)
x<-as.matrix(fbmsg[,1:53])
y<-as.matrix(fbmsg[,54])
fit<-lars(x,y,type = "lasso")
fit
summary(fit)

best_step<-fit$df[which.min(fit$RSS)]
best_step

predictions6<-predict(fit,x, s=best_step, type = "fit")$fit

mse6<-mean((y-predictions6)^2)
mse6

library(pls)

fit_pcr<-pcr(V54~., data=fbmsg, validation="CV")
fit_pcr
summary(fit_pcr)

predictions7<-predict(fit_pcr, fbmsg, ncomp = 6)
mse7<-mean((fbmsg$V54-predictions7)^2)
mse7

library(pls)
fit_pls<-plsr(V54~., data=fbmsg, validation="CV")
fit_pls
summary(fit_pls)

predictions8<-predict(fit_pls, fbmsg, ncomp = 6)


mse8<-mean((fbmsg$V54-predictions8)^2)
mse8

#4. create a graph displaying the accuracy of all models
# For plotting

plot(fit_lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit_ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit_elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

#####Sample 2




lib=c("bigmemory", "readr", "Hmisc", "dplyr", "MASS", "ggplot2", "lattice", "caret", "rpart", 
      "randomForest", "rpart.plot","lattice", "rattle", "data.table","RColorBrewer", "reshape2",
      "InformationValue","stringr", "VIF", "Information", "Amelia", "gdata", "party","car", 
      "lubridate","zoo", "sqldf", "fuzzyjoin", "party", "mice", "mlbench")
sapply(lib, require, character.only=TRUE, quietly=TRUE)

# import train data set


data1<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_1.csv")
data2<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_2.csv")
data3<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_3.csv")
data4<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_4.csv")
data5<- fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Training/Features_Variant_5.csv")

fbtrain<-rbind(data1,data2,data3,data4,data5)
dim(fbtrain)


#import test data set

test1<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_1.csv")
test2<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_2.csv")
test3<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_3.csv")
test4<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_4.csv")
test5<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_5.csv")
test6<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_6.csv")
test7<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_7.csv")
test8<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_8.csv")
test9<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_9.csv")
test10<-fread("C:/Users/Jagannath/Documents/assignment data acadgild/assignment 16-20/assignment 16-20/task 16 data/data task16/Testing/TestSet/Test_Case_10.csv")

#test10<-fread("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_10.csv", header = FALSE)

fbtest<-rbind(test1,test2,test3,test4,test5,test6,test7,test8,test9,test10)
dim(fbtest)

# Assign variable names to the train and test data set
colnames(fbtrain) <- c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
                       "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
                       "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
                       "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
                       "basetue","basewed","basethu","basefri","basesat","target")

colnames(fbtest) <- c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
                      "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
                      "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
                      "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
                      "basetue","basewed","basethu","basefri","basesat","target")

# exploratory analysis 
dim(fbtrain); dim(fbtest)
str(fbtrain); str(fbtest)
View(fbtrain); View(fbtest)
summary(fbtrain); summary(fbtest)
describe(fbtrain); describe(fbtest)
Amelia::missmap(fbtrain); Amelia::missmap(fbtest) # no missing values 

train<-(fbtrain); test<-(fbtest)
head(train); head(test)

# removing overlapping observations if any
distinct(train)
dim(train)
distinct(test)
dim(test)

# list the levels for the class
sapply(train, class)


# missing values count
sapply(train, function(x) sum(is.na(x))) # no missing values

# model building
library(lars)
x<-as.matrix(train[,c(30:34, 36:38)])
y<-as.matrix(train[,35])
fittrain<-lars(x,y,type = "lasso")

summary(fittrain)
plot(fittrain)

#result fit
fittrain

# select a step with a minimum error
best_step <- fittrain$df[which.min(fittrain$RSS)]
best_step
summary(best_step)

# predictions
predictions <- predict(fittrain, x, s=best_step, type="fit")$fittrain

# summarize accuracy
mse_train <- mean((y - predictions)^2)
print(mse_train)

# Elastic Net
install.packages("elasticnet")
library(elasticnet)

elasticnet::
  ENreg.fit(x_train,y_train)

pred_cv = ENreg.predict(x_cv)

#calculating mse

mse = np.mean((pred_cv - y_cv)*2)

#mse 1773750.73

ENreg.score(x_cv,y_cv)

# leaner model
LM<-lm(target~ x+y, data = train)
summary(LM)
attributes(LM)
plot(LM)
coef(LM)

plot(train$target, data=train, main="Scatterplot")


