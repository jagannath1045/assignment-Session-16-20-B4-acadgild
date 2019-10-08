# assignment 16-20 task 2 & 3(line 142 )
#task 2

weight_lifting_exercises<- weight_lifting
View('weight_lifting_exercises')
input<- weight_lifting_exercises
View(input)
input1<- as.numeric(input$new_window)
model<- glm(input1~raw_timestamp_part_1+raw_timestamp_part_2+cvtd_timestamp+num_window+roll_belt+pitch_belt+yaw_belt+total_accel_belt,data = input)
model
summary(model)
predict<- predict(model, type = "response")
head(predict, 5)
input$predict<- predict
input$predictROUND<- round(predict, digits = 0)
table(input$new_window, predict>= 0.5)
dim(input)
predict<- predict(model, type = "response")
head(predict, 5)


#------------------------Assignment 17 Task 2-----------------------------
# Weight Lifting Exercise Analysis

# Import Data Set

data_set <- read.csv("Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations task2.csv")
View(data_set)

# remove irrelevant collumns viz. name, cvtd_timestamp, new_window
data <- weight_lifting
View(data)
str(data)

# 2. Perform the below given activities:
# a. Create classification model using logistic regression model
# the target variable variable is multiple level 

sum(is.na(data))  # there are no missing values

# spliting the data set for train and test

library(caTools)
set.seed(123)
split = sample.split(data$classe, SplitRatio = 0.7) 

train = subset(data, split == TRUE)            # train data
test = subset(data, split == FALSE)            # test data

library(nnet) ; library(MASS)
model <- multinom(classe ~., data = train)
summary(model)

# stepAIC(model, direction = "backward")

final <- multinom(classe ~ raw_timestamp_part_1 + num_window + roll_belt + pitch_belt + 
                    yaw_belt + total_accel_belt + gyros_belt_x + gyros_belt_y + 
                    gyros_belt_z + accel_belt_x + accel_belt_y + accel_belt_z + 
                    magnet_belt_x + magnet_belt_y + magnet_belt_z + roll_arm + 
                    pitch_arm + yaw_arm + total_accel_arm + gyros_arm_x + gyros_arm_y + 
                    gyros_arm_z + accel_arm_x + accel_arm_y + accel_arm_z + magnet_arm_y + 
                    magnet_arm_z + roll_dumbbell + pitch_dumbbell + yaw_dumbbell + 
                    gyros_dumbbell_x + gyros_dumbbell_z + accel_dumbbell_x + 
                    accel_dumbbell_y + accel_dumbbell_z + magnet_dumbbell_x + 
                    magnet_dumbbell_y + magnet_dumbbell_z + roll_forearm + pitch_forearm + 
                    yaw_forearm + total_accel_forearm + gyros_forearm_x + gyros_forearm_y + 
                    gyros_forearm_z + accel_forearm_x + accel_forearm_y + accel_forearm_z + 
                    magnet_forearm_x + magnet_forearm_y + magnet_forearm_z, data = train)
final
summary(final)

# Predictions
predicted <- predict(final, newdata= test)

# ---------------------------------------------------------------------

# b. Goodness of Fit
library(car)
chisq.test(table(test$classe), prop.table(table(predicted)))

# --------------------------------------------------------------------------
# c. Report the accuracy measures
# Accuracy
conf <- table(test$classe, predicted)
OAA <- (conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4]+conf[5,5]) / sum(conf) 
OAA

# --------------------------------------------------------------------------
# d. Report the variable importance

coef(final)
library(caret)
varImp(final)

# --------------------------------------------------------------------------
# e. Report the unimportant variables
install.packages("Rtools")
install_github("riv","tomasgreif")
install_github("woe","tomasgreif")

library(devtools); library(woe); library(riv)

iv_df <- iv.mult(train, y = "classe", summary= FALSE, verbose = TRUE)

variables <- c(colnames(train[,-56]))
imp_variables <- names(as.data.frame(coef(final)))

unimportant_variables <- setdiff(variables, imp_variables)
unimportant_variables

# --------------------------------------------------------------------------
# f. Interpret the results

# 1. Model execution output shows some iteration history and includes the final negative log-likelihood 49.43.
# This value is multiplied by two as shown in the model summary as the Residual Deviance 98.86

# 2. The summary output has a block of coefficients and another block of standard errors. 
# Each blocks has row of values corresponding to each category of DV
# and each column represents the predictor
# and the values show the coefficients and standard errors

library(pROC)
m <- multiclass.roc(as.numeric(classe) ~ as.numeric(predicted) , data = test)
# AUC = 0.9977

# --------------------------------------------------------------------------
# g. Visualize the results

plot <- plot(conf, col = topo.colors(6))

library(ggplot2)
ggplot(data = as.data.frame(conf), mapping = aes(x = predicted,y = Var1)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red", trans = "log")




#task18....... task 3 ......

View(weight_lifting_exercises)
str(weight_lifting_exercises)
weight_lifting_exercises<-data.frame(weight_lifting_exercises[,-c(11:35,49:58,68:82,86:100,102:111,124:138,140:149)])

str(weight_lifting_exercises)
summary(weight_lifting_exercises)

weightTrain<-weight_lifting_exercises[1:2012,]
weightTest<-weight_lifting_exercises[2013:4024,]
summary(weightTrain)
names(weightTrain)


#Ques.2. Perform the below given activities:

# a. Create classification model using different decision trees.
weightTrain<-data.frame(weightTrain[,-c(11:35,49:58,68:82,86:100,102:111,124:138,140:149)])
library(caret)
library(Hmisc)
weightTrain$raw_timestamp_part_1<-impute(weightTrain$raw_timestamp_part_1,mean)
weightTrain$raw_timestamp_part_2<-impute(weightTrain$raw_timestamp_part_2,mean)
weightTrain$cvtd_timestamp<-impute(weightTrain$cvtd_timestamp,mean)
weightTrain$new_window<-impute(weightTrain$new_window,mean)
weightTrain$num_window<-impute(weightTrain$num_window,mean)
weightTrain$roll_belt<-impute(weightTrain$roll_belt,mean)
weightTrain$pitch_belt<-impute(weightTrain$pitch_belt,mean)
weightTrain$yaw_belt<-impute(weightTrain$yaw_belt,mean)
summary(weightTrain)
str(weightTrain)

weightTrain$cvtd_timestamp<-as.integer(weightTrain$cvtd_timestamp)
weightTrain$new_window<-as.integer(weightTrain$new_window)
library(tree)
tree<-tree(classe~. , 
           data = weightTrain)
plot(tree,pretty = 0.1)
text(tree,pretty = 1.2)
summary(tree)

library(caret)
pred <- predict(tree,weightTrain,type='class')
str(pred)
dim(pred)
dim(weightTest$classe)

weightTest$classe<-as.factor(weightTest$classe)
dim(weightTest$classe)
table(weightTest$classe,pred)

length(pred)
length(weightTest$classe)
confusionMatrix(pred,weightTest$classe)
#.........

install.packages("rpart")
library(rpart)
fit1 <- rpart(classe~.,data=weightTrain[,-1]) 
class(fit1)
summary(fit1)

rpart.plot::rpart.plot(fit1)

pred1<-predict(fit1,weightTrain,type = "class")
summary(pred1)
dim(pred1)
weightTest$classe<-as.factor(weightTest$classe)
table(weightTest$classe,pred1)
confusionMatrix(weightTest$classe,pred1)



# b. Verify model goodness of fit.
#........for pred.....
weightTest$classe<-as.factor(weightTest$classe)
dim(weightTest$classe)
table(weightTest$classe,pred)

length(pred)
length(weightTest$classe)
confusionMatrix(pred,weightTest$classe)


#...for fit1....
weightTest$classe<-as.factor(weightTest$classe)
table(weightTest$classe,pred1)
confusionMatrix(weightTest$classe,pred1)


# c. Apply all the model validation techniques.

set.seed(3)
install.packages('tree')
library(tree)
cv.weight<-cv.tree(tree,FUN = prune.misclass)    #cv->cross validation
cv.weight_lifting_exercises<-cv.tree(tree,FUN = prune.misclass)
names(cv.weight)
cv.weight

par(mfrow = c(1,2))
plot(cv.weight$size,cv.weight$dev,type = 'b',col = 'red')

prune.weight<-prune.misclass(tree,best = 9)
plot(prune.weight)
text(prune.weight,pretty = 0)

weightTrain$cvtd_timestamp<-as.integer(weightTrain$cvtd_timestamp)
weightTrain$new_window<-as.integer(weightTrain$new_window)
tree.pred1<-predict(prune.weight,weightTrain,type = 'class')
table(tree.pred1,weightTest)



#............Random forest.........
library(randomForest)
set.seed(1)
a.weight_lifting_exercises<-randomForest(classe~.,weight_lifting_exercises,
                                         subset = weightTrain,mtry = 3,importance = TRUE)
dim(a.weight_lifting_exercises)

importance(a.weight_lifting_exercises)

varImpPlot(a.weight_lifting_exercises,col = 'blue',pch = 10, cex = 1.25)

a.weight_lifting_exercises

test.pred.rf<-predict(a.weight_lifting_exercises, newdata = weight_lifting_exercises[-weightTrain,],type = 'class')
table(test.pred.rf,weightTest)


#.........adaboost..........

install.packages("adabag")
library(adabag)
set.seed(300)
weight_lifting_exercises$classe<-as.character(weight_lifting_exercises$classe)
weight_adaboost<-boosting(classe~., data = weight_lifting_exercises)

p.weight_adaboost<-predict(weight_adaboost,weight_lifting_exercises)
head(p.weight_adaboost)
head(p.weight_adaboost$class)
p.weight_adaboost$confusion
set.seed(300)
car_adaboost_cv<-boosting.cv(classe,data = weight_lifting_exercises)
car_adaboost_cv$confusion


