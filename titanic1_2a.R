# Applies RF to "Survived","Sex","Age","SibSp","Parch","clsEmbk"
# with 80% trainging data yields
# Accuracy : 0.8418 , Sensitivity : 0.8462  , and Specificity :0.8333 .

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

first <- which(titanic$Pclass==1)
second <- which(titanic$Pclass==2)
third <- which(titanic$Pclass==3)
cherbourg <- which(titanic$Embarked=="C")
queenstown <- which(titanic$Embarked=="Q")
southampton <- which(titanic$Embarked=="S")

titanic <- cbind(titanic,clsEmbk=c(0))
titanic$clsEmbk[intersect(cherbourg,first)] <- "cherbourg1"
titanic$clsEmbk[intersect(cherbourg,second)] <- "cherbourg2"
titanic$clsEmbk[intersect(cherbourg,third)] <- "cherbourg3"
titanic$clsEmbk[intersect(queenstown,first)] <- "queenstown1"
titanic$clsEmbk[intersect(queenstown,second)] <- "queenstown2"
titanic$clsEmbk[intersect(queenstown,third)] <- "queenstown3"
titanic$clsEmbk[intersect(southampton,first)] <- "southampton1"
titanic$clsEmbk[intersect(southampton,second)] <- "southampton2"
titanic$clsEmbk[intersect(southampton,third)] <- "southampton3"
titanic$clsEmbk <- as.factor(titanic$clsEmbk)

# Includes features
names(titanic)
titanic <- titanic[,c(2,5:8,13)]


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