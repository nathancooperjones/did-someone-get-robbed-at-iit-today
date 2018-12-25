
library(tidyverse) 
library(caret) 
library(lattice) 
library(randomForest) 

##### Load crime dataset 
crimeData <- read.csv("~/Desktop/CSP_571_Project/final_data.csv") 
crimeData <- as.tibble(crimeData) 

crimeData$Date <- as.POSIXct(crimeData$Date) 
crimeData$Arrest <- as.factor(crimeData$Arrest) 
crimeData$Domestic <- as.factor(crimeData$Domestic) 
crimeData$Quadrant <- as.factor(crimeData$Quadrant) 
crimeData$Month <- as.factor(crimeData$Month) 
crimeData$Year <- as.factor(crimeData$Year) 


#Check for observations 
nrow(crimeData)
#check if there is missing data
crimeData <- crimeData[, !(colnames(crimeData) %in% c("Description"))] # obviously can't have that 
sum(!complete.cases(crimeData))
summary(crimeData)
head(crimeData)
str(crimeData) 

#####Create a Validation Dataset 

# set.seed(1) 
trainIndex  <- createDataPartition(crimeData$Primary.Type,p=0.8,list=F,times=1) 

train <- crimeData[trainIndex,]
test  <- crimeData[-trainIndex,]

####Predicting crime with temperature 

#Use all other variables to predict primary.type 
library(randomForest)
#rf <- randomForest:::predict.randomForest(Primary.Type~.,data=train,ntree=100,proximity = TRUE)
train <- train[, !colnames(train) %in% c("Semester")] 
unique(train$Location.Description) 
rf <- randomForest(Primary.Type~., data=train, ntree=100, proximity=TRUE)
summary(rf) 

plot(rf) 


# Accuracy for training 
known <- train$Primary.Type 
unknown <- predict(rf, train) 

accuracy_vec <- known == unknown 

accuracy <- length(accuracy_vec[accuracy_vec == TRUE]) / length(accuracy_vec) 
misclassification_rate <- 1 - accuracy 

print("TRAINING: ") 
cat(paste0("Accuracy: ", accuracy, "\nMisclassifcation Rate: ", misclassification_rate, "\n")) 

# Accuracy for testing 
known <- test$Primary.Type 
unknown <- predict(rf, test) 

accuracy_vec <- known == unknown 

accuracy <- length(accuracy_vec[accuracy_vec == TRUE]) / length(accuracy_vec) 
misclassification_rate <- 1 - accuracy 

print("TESTING: ") 
cat(paste0("Accuracy: ", accuracy, "\nMisclassifcation Rate: ", misclassification_rate, "\n"))  



getTree(rf, 1, labelVar=TRUE) 

getTree(rf, k=1, labelVar=TRUE) 







## NORMAL TREE 
library(rpart) 
library(rpart.plot) 
tree <- rpart(Primary.Type~., data = train, method = "class") 

rpart.plot(tree, box.palette=0) 

sum(predict(tree, test, type = "class") == test$Primary.Type) / nrow(test) * 100 

