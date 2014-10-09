# Applies RF to "Survived","Age","SibSp","Parch","Embarked","clsSex"
# with 80% trainging data yields
# Accuracy : 0.8531 , Sensitivity : 0.8547  , and Specificity :0.8500 .

library(caret); library(randomForest)

# Loads and preprocesses the data
titanic <- read.csv("train.csv")

# Imputes missing ages with the median age of the respective sex
ageFImpute <- median(titanic$Age[titanic$Sex=="female" & !is.na(titanic$Age)])
titanic$Age[titanic$Sex=="female" & is.na(titanic$Age)] <- ageFImpute
ageMImpute <- median(titanic$Age[titanic$Sex=="male" & !is.na(titanic$Age)])
titanic$Age[titanic$Sex=="male" & is.na(titanic$Age)] <- ageMImpute   

# Imputes missing embarked port with the mode
titanic$Embarked[titanic$Embarked==""] <- "S"


# Combines the Pclass and Embarked covariates
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)

male <- which(titanic$Sex=="male")
female <- which(titanic$Sex=="female")
first <- which(titanic$Pclass==1)
second <- which(titanic$Pclass==2)
third <- which(titanic$Pclass==3)


titanic <- cbind(titanic,clsSex=c(0))
titanic$clsSex[intersect(male,first)] <- "male1"
titanic$clsSex[intersect(male,second)] <- "male2"
titanic$clsSex[intersect(male,third)] <- "male3"
titanic$clsSex[intersect(female,first)] <- "female1"
titanic$clsSex[intersect(female,second)] <- "female2"
titanic$clsSex[intersect(female,third)] <- "female3"
titanic$clsSex <- as.factor(titanic$clsSex)

# Includes features
names(titanic)
titanic <- titanic[,c(2,6:8,12,13)]


# Partitions the data into training and test sets
set.seed(314)
inTrain <- createDataPartition(y=titanic$Survived, p=0.8, list=FALSE)
training <- titanic[inTrain,]
testing <- titanic[-inTrain,]


# Fits a model and predicts using the model
modFit <- randomForest(Survived~ ., data=training, importance=TRUE, proximity=TRUE)
# Displays the importance of each predictor.
importance(modFit)
# Tests on validation set 1.
prediction <- predict(modFit,testing[,-1])
confusionMatrix(testing$Survived, prediction)