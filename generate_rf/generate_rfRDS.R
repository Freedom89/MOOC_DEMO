#set working directory 

#setwd("enter yourself")

library(randomForest)
library(dplyr)
data_train <- read.csv("Practical Machine Learning _ Coursera.csv", na.strings = c("","NA"))
data_test <- read.csv("pml-testing.csv")

#find out how many NA entries are there in each column 
checker=colSums(is.na(data_train))
checker
#summarise the result
table(checker)
#find out which col names to be removed - with a count greater than 15000 
names_to_remove<-names(checker)[checker>15000]

data_train <- data_train %>% select(-one_of(names_to_remove))
data_test <- data_test %>% select(-one_of(names_to_remove))

#remove first 6 columns - because it is not required
data_train <- data_train[,c(7:60)]
data_test <- data_test[,c(7:60)]

#also remove proelbme id from data_test 
data_test <- data_test %>% select(-problem_id)
#change classe into factor
data_train <- data_train %>% mutate(classe = as.factor(classe))

#train the randomForest model
#skip this part
ptm<- proc.time()
rf_model1 <- randomForest(classe~., data=data_train,
                          ntree = 500,
                          mtry  = 7,
                          importance = TRUE)
proc.time() - ptm
#time taken to run is about 6 minutes - still reasonable, compared to using caret
#save the file - will go thru this later. 
saveRDS(rf_model1,"rf.model1.RDS")

rf_model1 <- readRDS("rf.model1.RDS")
#can predict probabilities         
predict(rf_model1, data_test, type = 'prob')

#can predict the class 
predict(rf_model1, data_test)

model_answer <- predict(rf_model1,data_test)
#correct ans from the Machine Learning Course 
correct_ans <- c("B","A","B","A","A","E","D","B","A","A","B","C","B","A","E","E","A","B","B","B")

#compare my answer with the true answer 
model_answer == correct_ans
sum(model_answer==correct_ans)

#plot variable importance
varImpPlot(rf_model1)

#choose MeanDecreaseGini - can read up more about it at wikipedia 
impt_var<-importance(rf_model1,2)

#order according to importance 
impt_var <- impt_var[order(-impt_var),]

#extract the top 12 - no particular reason for this number 12
names_top12 <- names(impt_var)[1:12]

#filter the training data and test data with the remaining 12 features 
#remember to include the target variable in the data_train 
data_train_new <- data_train %>% select(one_of(names_top12,"classe"))
data_test_new <- data_test %>% select(one_of(names_top12))

ptm <- proc.time()
rf_model2 <- randomForest(classe~., data=data_train_new,
                          ntree = 500,
                          mtry  = 7,
                          importance = TRUE)
proc.time() - ptm
#new time taken is 50 seconds in total 

predict(rf_model2, data_test_new)
predict(rf_model2, data_test_new) == correct_ans
sum(predict(rf_model2, data_test_new) == correct_ans)
varImpPlot(rf_model2)

#prepare shiny model - for image saving
remove(names_to_remove,
       data_test,
       data_train,
       data_train_new,
       checker,
       impt_var,
       rf_model1,
       correct_ans,
       names_top12,
       ptm,
       model_answer)

#can save as image file 
save.image(file = "MOOC_image.RData")

#save it as a single file 
saveRDS(rf_model2,"RF_12v.RDS")
