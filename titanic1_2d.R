# Applies RF to "Survived","Age","SibSp","Parch","clsEmbkSex"
# with 80% trainging data yields
# Accuracy : 0.8588 , Sensitivity : 0.8621  , and Specificity :0.8525.

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


# Combines the Pclass, Embarked and Sex covariates
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)

male <- which(titanic$Sex=="male")
female <- which(titanic$Sex=="female")
first <- which(titanic$Pclass==1)
second <- which(titanic$Pclass==2)
third <- which(titanic$Pclass==3)
cherbourg <- which(titanic$Embarked=="C")
queenstown <- which(titanic$Embarked=="Q")
southampton <- which(titanic$Embarked=="S")

titanic <- cbind(titanic,clsEmbkSex=c(0))

titanic$clsEmbkSex[intersect(intersect(cherbourg,first),male)] <- "cherbourg1M"
titanic$clsEmbkSex[intersect(intersect(cherbourg,second),male)] <- "cherbourg2M"
titanic$clsEmbkSex[intersect(intersect(cherbourg,third),male)] <- "cherbourg3M"
titanic$clsEmbkSex[intersect(intersect(queenstown,first),male)] <- "queenstown1M"
titanic$clsEmbkSex[intersect(intersect(queenstown,second),male)] <- "queenstown2M"
titanic$clsEmbkSex[intersect(intersect(queenstown,third),male)] <- "queenstown3M"
titanic$clsEmbkSex[intersect(intersect(southampton,first),male)] <- "southampton1M"
titanic$clsEmbkSex[intersect(intersect(southampton,second),male)] <- "southampton2M"
titanic$clsEmbkSex[intersect(intersect(southampton,third),male)] <- "southampton3M"

titanic$clsEmbkSex[intersect(intersect(cherbourg,first),female)] <- "cherbourg1F"
titanic$clsEmbkSex[intersect(intersect(cherbourg,second),female)] <- "cherbourg2F"
titanic$clsEmbkSex[intersect(intersect(cherbourg,third),female)] <- "cherbourg3F"
titanic$clsEmbkSex[intersect(intersect(queenstown,first),female)] <- "queenstown1F"
titanic$clsEmbkSex[intersect(intersect(queenstown,second),female)] <- "queenstown2F"
titanic$clsEmbkSex[intersect(intersect(queenstown,third),female)] <- "queenstown3F"
titanic$clsEmbkSex[intersect(intersect(southampton,first),female)] <- "southampton1F"
titanic$clsEmbkSex[intersect(intersect(southampton,second),female)] <- "southampton2F"
titanic$clsEmbkSex[intersect(intersect(southampton,third),female)] <- "southampton3F"

titanic$clsEmbkSex <- as.factor(titanic$clsEmbkSex)


# Cuts the ages into groups of 5 years
titanic <- cbind(titanic,ageG=cut(titanic$Age, breaks=5*(0:16)))


# Calculates the family size
titanic <- cbind(titanic,famSize=titanic$SibSp+titanic$Parch+1)


# Extracts titles and lastnames
names <- as.character(titanic$Name)
lastname <- sapply(names,function(x) strsplit(x,",")[[1]][1])
lastname <- as.vector(lastnames)
title <- sapply(names,function(x) strsplit(x,",")[[1]][2])
title <- as.vector(title)
title <- sapply(title,function(x) strsplit(x," ")[[1]][2])
title <- as.vector(title)
title <- as.factor(title)
titanic <- cbind(titanic,title)

# Includes features
names(titanic)
titanic <- titanic[,c(2,6,7,8,13,16)]
#(2,6,13,15): (Survived   Age    clsEmbkSex famSize) Accuracy : 0.8249 
#(2,13,14,15): ("Survived"   "clsEmbkSex" "ageG"       "famSize"  ) Accuracy : 0.8249 
#(2,10,13,14,15): ("Survived"   "Fare"       "clsEmbkSex" "ageG"       "famSize"   )  Accuracy : 0.7966
#(2,13,14,15,16): ( "Survived"   "clsEmbkSex" "ageG"       "famSize"    "title"   ) Accuracy : 0.8136  
#(2,7,8,13,14,16):("Survived"  "SibSp"   "Parch"   "clsEmbkSex" "ageG" "title" ) Accuracy : 0.8192  
#(2,6,7,8,13,16):("Survived" "Age" "SibSp" "Parch" "clsEmbkSex" "title" ) Accuracy : 0.8305 

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



qplot(Age,clsEmbkSex, colour=Survived, data=training)