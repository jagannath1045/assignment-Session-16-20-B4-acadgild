#task 19 trial 1
#nn_classifiers
install.packages
library(Matrix)
View(weight_lifting)
abcd <- weight_lifting
View(abcd)           
dim(abcd)
str(abcd)

table(abcd$user_name)
barplot(table(abcd$user_name))

barplot(table(abcd$user_name), col = c('green', 'red', 'blue', 'yellow', 'pink'),
        main = 'Bar Plot of user name')

text(barplot(table(abcd$user_name), col = c('green', 'red', 'blue', 'yellow', 'pink'),
             main = 'Bar Plot of user name'), 0,  
     table(abcd$user_name), cex = 2, pos = 3)


# recode diagnosis variable
abcd$user_name<- factor(abcd$user_name, levels = c('A', 'C', 'E', 'J', 'P'), 
                        labels = c('Adelmo', 'Carlitos', 'Eurico' , 'Jeremy', 'Pedro'))
table(abcd$user_name)
# replot the bar plot
text(barplot(table(abcd$user_name), col = c('green', 'red', 'blue', 'yellow', 'pink'),
             main = 'Bar Plot of user name'), 0,
     table(abcd$user_name), cex = 2, pos = 3)
# proportion
round(prop.table(table(abcd$user_name))*100, digits =2)

# normalization
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

# lapply() takes a list and applies a specified function 
# to each item in the list
# start from 2 bcz 1st is a factor (diagnosis)
cvtd_timestamp<- as.numeric(abcd$ cvtd_timestamp)
new_window<- as.numeric(abcd$new_window)
kurtosis_picth_belt <- as.numeric(abcd$kurtosis_picth_belt)
kurtosis_yaw_belt <- as.numeric(abcd$kurtosis_yaw_belt)
skewness_roll_belt.1<- as.numeric(abcd$skewness_roll_belt.1)
skewness_yaw_belt<- as.numeric(abcd$skewness_yaw_belt)
kurtosis_roll_arm<- as.numeric(abcd$kurtosis_roll_arm)
kurtosis_pitch_arm<- as.numeric(abcd$kurtosis_pitch_arm)
kurtosis_yaw_arm<- as.numeric(abcd$kurtosis_yaw_arm)
skewness_roll_arm<- as.numeric(abcd$skewness_roll_arm)
skewness_pitch_arm<- as.numeric(abcd$skewness_pitch_arm)
skewness_yaw_arm<- as.numeric(abcd$skewness_yaw_arm)
kurtosis_yaw_dumbbell<- as.numeric(abcd$kurtosis_yaw_dumbbell)
skewness_yaw_dumbbell<- as.numeric(abcd$ skewness_yaw_dumbbell)
kurtosis_roll_forearm<- as.numeric(abcd$kurtosis_roll_forearm)
kurtosis_pitch_forearm<- as.numeric(abcd$kurtosis_pitch_forearm)
kurtosis_yaw_forearm<- as.numeric(abcd$kurtosis_yaw_forearm)
skewness_roll_forearm<- as.numeric(abcd$skewness_roll_forearm)
skewness_pitch_forearm<- as.numeric(abcd$skewness_pitch_forearm)
skewness_yaw_forearm<- as.numeric(abcd$skewness_yaw_forearm)
max_yaw_forearm<- as.numeric(abcd$max_yaw_forearm) 
min_yaw_forearm<- as.numeric(abcd$min_yaw_forearm)
amplitude_yaw_forearm<- as.numeric(abcd$amplitude_yaw_forearm)
classe<- as.numeric(abcd$classe)

abcd_n<- as.data.frame(lapply(abcd[2:159], normalize))
wbcd_n<- as.data.frame(lapply(wbcd[2:31], normalize))                       
str(abcd_n)
str(abcd)

# train and test data sets, 

abcd_train<- abcd_n[1:3500, ]
abcd_test<- abcd_n[3501:4024, ]

# how we will compare the membership?
# label vector
abcd_train_labels<- abcd[1:3500, 1]
abcd_test_labels<- abcd[3501:4024, 1]

str(abcd_train)
str(abcd_train_labels)

str(abcd_test)
str(abcd_test_labels)

# training
install.packages('class')
library(class)

# wow! training and testing simultaneously!
# choose k as approx sqrt of n in training data set
# sqrt 469 = approx 21

abcd_test_pred<- knn(train = abcd_train, test = abcd_test,
                     cl = abcd_train_labels, k= 61)

summary(abcd_test_pred)

# evaluate model performance

install.packages('gmodels')
library(gmodels)

(CrossTable(x = abcd_test_labels, y = abcd_test_pred,
            prop.chisq = FALSE))

#ann classification
# before normnalizing, diagnosis has to be an integer (or numeric)
abcd$user_name<- as.integer(abcd$user_name)
str(abcd)

normalize<- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

abcd_n<- as.data.frame(lapply(abcd[2:159], normalize))
# id is not normalised
str(abcd_n)
train_ann<- abcd_n[1:3500,]
test_ann<- wbcd_n[3501:4024,]

str(train_ann)

library(neuralnet)

ann_train<- neuralnet(user_name~raw_timestamp_part_1+raw_timestamp_part_2+cvtd_timestamp+new_window+num_window+roll_belt+pitch_belt+yaw_belt+
                        total_accel_belt+gyros_belt_x+gyros_belt_y+gyros_belt_z+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+
                        pitch_arm+yaw_arm+total_accel_arm+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+
                        roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell+gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z+accel_dumbbell_x+accel_dumbbell_y+
                        accel_dumbbell_z+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm+gyros_forearm_x+
                        gyros_forearm_y+gyros_forearm_z+accel_forearm_x+accel_forearm_y+accel_forearm_z+magnet_forearm_x+magnet_forearm_y+magnet_forearm_z+classe,
                      data = train_ann,
                      hidden = c(2,1),
                      linear.output = FALSE,
                      threshold = 0.01)


ann_train$result.matrix

plot(ann_train)

str(test_ann)
test.ann<- subset(test_ann[,-1])
str(test.ann)

ann.results<- compute(ann_train, test.ann)
head(ann.results$net.result, 5)

# Accuracy
results<- data.frame(actual = test_ann$user_name,
                     prediction = ann.results$net.result)

roundedresults<- sapply(results, round, digits = 0)

roundedresults$df<- data.frame(roundedresults)

attach(roundedresults$df)

table(actual, prediction)

dim(test_ann)

# letters classifier
abcd <- read.csv("C:/Users/satish/Desktop/abcd.csv")
View(abcd)
letters<- abcd
dim(letters)
str(letters)
names(letters)

letters_train<- letters[1:3500, ]
letters_test<- letters[3501:4024, ]

install.packages('kernlab')
library(kernlab)

letter_classifier<- ksvm(user_name~., data = letters_train,
                         kernel = 'vanilladot')
letter_classifier  

#test
letter_predictions<- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)

agreement<- letter_predictions == letters_test$letter

table(agreement) 
prop.table(table(agreement)) 

#____improving model
set.seed(12345)
letter_classifier_rbf<- ksvm(user_name~., data = letters_train,
                             kernel = 'rbfdot')
# it takes 1 to 3 minutes, mine is 16 GB RAM

letter_predictions_rbf<- predict(letter_classifier_rbf,
                                 letters_test)

agreement_rbf<- letter_predictions_rbf == letters_test$letter

table(agreement_rbf) 
prop.table(table(agreement_rbf)) 



