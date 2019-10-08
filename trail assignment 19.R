#task 4 (session 16-20) assignment 

#### trial 2 
#------------------------Assignment 19 -----------------------------

# a. Create classification model using different classifiers

data_set <- weight_lifting
View(data_set)

# remove irrelevant collumns viz. name, cvtd_timestamp, new_window
data <- data_set[,-c(1,4,5)]
View(data)
str(data)

sum(is.na(data))  # there are no missing values

# spliting the data set for train and test

library(caTools)
set.seed(123)
split = sample.split(data$classe, SplitRatio = 0.7) 

train = subset(data, split == TRUE)            # train data
test = subset(data, split == FALSE)            # test data

# a. Create classification model using different classifiers

library(tree); library(rpart); library(caret); library(C50); library(randomForest)
library(adabag); library(gbm)

# bagging -----------------------------------

model_bag <- bagging(classe ~., data = train , mfinal = 10)     # model
model_bag$importance
pred_bag <- predict.bagging(model_bag, newdata = test)          # make prediction
pred_bag
pred_bag$confusion                                              # confusion matrix
1-pred_bag$error                                                # accuracy


# Boosting ---------------------------------
model_boost <- boosting(classe ~., data = train, mfinal= 10, coeflearn = "Freund",
                        boos = FALSE, control = rpart.control(maxdepth = 3))  # model
model_boost$importance
pred_boost <- predict.boosting(model_boost, newdata = test)     # make prediction
pred_boost$confusion                                            # confusion matrix
1-pred_boost$error                                              # accuracy


# Gradient Boosting----------------------------
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = 'grid')
rf_gbm <- train(classe ~ ., data = train, trControl = train_control, method = "gbm",
                metric = 'Accuracy') 
print(rf_gbm)
plot(rf_gbm)
pred_rf_gbm <- predict(rf_gbm, test)                             # make prediction
conf_rf_gbm <- confusionMatrix(test$classe, pred_rf_gbm)         # confusion matrix
conf_rf_gbm$overall[1]                                           # accuracy
summary(rf_gbm)                                                  # var importance - 18


# class -----------------------------
model_class <- tree(classe ~., data = train)              # model
summary(model_class)
plot(model_class); text(model_class)
pred_class <- predict(model_class, test, type = 'class')  # make prediction
conf_class <- confusionMatrix(test$classe, pred_class)    # confusion matrix
conf_class
conf_class$overall[1]

# Random forest ---------------------
model_rf <- randomForest(classe ~., train, ntree = 500)
model_rf
pred_rf <- predict(model_rf, test)                       # make prediction
conf_rf <- confusionMatrix(test$classe, pred_rf)    # confusion matrix
conf_rf
conf_rf$overall[1]

# Boosted Tree ---------------------
train_control <- trainControl(method = "cv", number = 10)
model_bst <- train(classe ~ ., data = train, trControl = train_control, method = "bstTree")
model_bst
pred_bst <- predict(model_bst, test)                       # make prediction
conf_bst <- confusionMatrix(test$classe, pred_bst)    # confusion matrix
conf_bst
conf_bst$overall[1]


# CART ----------------------------
model_cart <- rpart(classe ~ ., data = train)           # model
summary(model_cart)
rpart.plot::rpart.plot(model_cart)
plotcp(model_cart)
pred_cart <- predict(model_cart, test, type = 'class')  # make prediction
conf_cart <- confusionMatrix(test$classe, pred_cart)    # confusion matrix
conf_cart
conf_cart$overall[1]

# CV ------------------------------
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(classe ~ ., data = train, trControl = train_control, method = "rpart")
model_cv
pred_cv <- predict(model_cv, test)                       # make prediction
conf_cv <- confusionMatrix(test$classe, pred_cv)         # confusion matrix
conf_cv
conf_cv$overall[1]

# Ross Quinlan C5.0 ----------------
train_control <- trainControl(method = "cv", number = 10)
model_c5.0 <- train(classe ~ ., data = train, trControl = train_control, method = "C5.0")
model_c5.0
pred_c5.0 <- predict(model_c5.0, test)                       # make prediction
conf_c5.0 <- confusionMatrix(test$classe, pred_c5.0)         # confusion matrix
conf_c5.0
conf_c5.0$overall[1]



# C5.0 Rules
train_control <- trainControl(method = "cv", number = 10)
model_c5.0rules <- train(classe ~ ., data = train, trControl = train_control, method = "C5.0Rules")
model_c5.0rules
pred_c5.0rules <- predict(model_c5.0rules, test)                       # make prediction
conf_c5.0rules <- confusionMatrix(test$classe, pred_c5.0rules)         # confusion matrix
conf_c5.0rules
conf_c5.0rules$overall[1]

# C5.0 Tree
train_control <- trainControl(method = "cv", number = 10)
model_c5.0tree <- train(classe ~ ., data = train, trControl = train_control, method = "C5.0Tree")
model_c5.0tree
pred_c5.0tree <- predict(model_c5.0tree, test)                       # make prediction
conf_c5.0tree <- confusionMatrix(test$classe, pred_c5.0tree)         # confusion matrix
conf_c5.0tree
conf_c5.0tree$overall[1]

# conditional inference trees
# Ctree
train_control <- trainControl(method = "cv", number = 10)
model_ctree <- train(classe ~ ., data = train, trControl = train_control, method = "ctree")
model_ctree
pred_ctree <- predict(model_ctree, test)                       # make prediction
conf_ctree <- confusionMatrix(test$classe, pred_ctree)         # confusion matrix
conf_ctree
conf_ctree$overall[1]

# Ctree2
train_control <- trainControl(method = "cv", number = 10)
model_ctree2 <- train(classe ~ ., data = train, trControl = train_control, method = "ctree2")
model_ctree2
pred_ctree2 <- predict(model_ctree2, test)                       # make prediction
conf_ctree2 <- confusionMatrix(test$classe, pred_ctree2)         # confusion matrix
conf_ctree2
conf_ctree2$overall[1]


#------------------------------------------------------------------------------------------
# b. Verify model goodness of fit.

chisq.test(table(test$classe), prop.table(table(pred_bag$class)))  # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_boost$class)))# pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_rf_gbm)))     # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_rf)))         # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_bst)))        # pv = 0.2650 
chisq.test(table(test$classe), prop.table(table(pred_cart)))       # pv = 0.2202 
chisq.test(table(test$classe), prop.table(table(pred_cv)))         # pv = 0.2414 
chisq.test(table(test$classe), prop.table(table(pred_c5.0)))       # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_c5.0rules)))  # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_c5.0tree)))   # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_ctree)))      # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_ctree2)))     # pv = 0.2202

1- pred_bag$error          # 0.9809603
1- pred_boost$error        # 0.9991722
conf_rf_gbm$overall[1]     # 1
conf_class$overall[1]      # 0.9511589
conf_rf$overall[1]         # 1
conf_bst$overall[1]        # 0.5612583
conf_cart$overall[1]       # 0.968543
conf_cv$overall[1]         # 0.8807947
conf_c5.0$overall[1]       # 1
conf_c5.0rules$overall[1]  # 1
conf_c5.0tree$overall[1]   # 1
conf_ctree$overall[1]      # 1
conf_ctree2$overall[1]     # 0.9312914

#-----------------------------------------------------------------------------------------
# c. Apply all the model validation techniques.

# Performing cross-validation with the bagging method
# we use bagging.cv to make a 10-fold classification on the training dataset with 10 iterations:

model_bag_cv <- bagging.cv(classe ~ ., data = train, v=10, mfinal = 10)
model_bag_cv$confusion
model_bag_cv$error

# Performing cross-validation with the boosting method
model_boost_cv <- boosting.cv(classe ~ ., data = train, v=10, mfinal = 10,
                              control = rpart.control(cp=0.01))
model_boost_cv$confusion
model_boost_cv$error

# 1
train_control <- trainControl(method = "cv", number = 10)
cvmodel1 <- train(classe ~ ., data = train, trControl = train_control, method = "rf") 
cvpred1 <- predict(cvmodel1, test)                        # make prediction
cvconf1 <- confusionMatrix(test$classe, pred_ctree)       # confusion matrix
cvconf1$overall[1]                                        # accuracy

# default
set.seed(123)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
rf_default <- train(classe ~ ., data = train, trControl = train_control, method = "rf",
                    metric = 'Accuracy', tuneGrid = expand.grid(.mtry = sqrt(ncol(train)))) 
pred_rf_default <- predict(rf_default, test)                            # make prediction
conf_rf_default <- confusionMatrix(test$classe, pred_rf_default)        # confusion matrix
conf_rf_default$overall[1]                                              # accuracy
varImp(rf_default)                                                      # var importance - 20

# random search for parameters
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = 'random')
rf_random <- train(classe ~ ., data = train, trControl = train_control, method = "rf",
                   metric = 'Accuracy', tuneLength = 15) 
pred_rf_random <- predict(rf_random, test)                            # make prediction
conf_rf_random <- confusionMatrix(test$classe, pred_rf_random)        # confusion matrix
conf_rf_random$overall[1]                                             # accuracy
varImp(rf_random)                                                     # var importance - 20

# Grid Search
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = 'grid')
rf_grid <- train(classe ~ ., data = train, trControl = train_control, method = "rf",
                 metric = 'Accuracy', tuneGrid = expand.grid(.mtry=c(1:15))) 
pred_rf_grid <- predict(rf_grid, test)                            # make prediction
conf_rf_grid <- confusionMatrix(test$classe, pred_rf_grid)        # confusion matrix
conf_rf_grid$overall[1]                                           # accuracy
varImp(rf_grid)                                                   # var importance - 20

# gradient boosting
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = 'grid')
rf_gbm <- train(classe ~ ., data = train, trControl = train_control, method = "gbm",
                metric = 'Accuracy') 
print(rf_gbm)
plot(rf_gbm)
pred_rf_gbm <- predict(rf_gbm, test)                             # make prediction
conf_rf_gbm <- confusionMatrix(test$classe, pred_rf_gbm)         # confusion matrix
conf_rf_gbm$overall[1]                                           # accuracy
summary(rf_gbm)                                                  # var importance - 18


# Problem was to predict how well the activity is performed
# The target variable is the 5 classe; 1 accurate and 4 type of error 
# occured during the activity

# error (target) detection was done by classifying an 
# execution to one of the mistake classes

# we could detect mistakes fairly accurately

# Gradient bossting model is most accurate with less number of predictors 
# Model is good fit and the Accuracy is 1

plot <- plot(conf_rf$table, col = topo.colors(6))



