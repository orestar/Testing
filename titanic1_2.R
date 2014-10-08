# Applies RF directly to "Survived","Pclass","Sex","Age","SibSp","Parch","Embarked"
# with 80% trainging data yields
# Accuracy : 0.8531 , Sensitivity : 0.8547 , and Specificity : 0.8500 .

library(caret); library(randomForest)

titanic <- read.csv("train.csv")
summary(titanic)
names(titanic)
titanic <- titanic[,-c(1,4,9:11)]


titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
ageFImpute <- median(titanic$Age[titanic$Sex=="female" & !is.na(titanic$Age)])
titanic$Age[titanic$Sex=="female" & is.na(titanic$Age)] <- ageFImpute
ageMImpute <- median(titanic$Age[titanic$Sex=="male" & !is.na(titanic$Age)])
titanic$Age[titanic$Sex=="male" & is.na(titanic$Age)] <- ageMImpute   
titanic$Embarked[titanic$Embarked==""] <- "S"  

set.seed(314)
inTrain <- createDataPartition(y=titanic$Survived, p=0.8, list=FALSE)
training <- titanic[inTrain,]
testing <- titanic[-inTrain,]


xyplot(Survived~Age|Pclass, training)
xyplot(Survived~Age|Embarked, training)

firstM <- training[training$Pclass==1 & training$Sex=="male", ]
secondM <- training[training$Pclass==2 & training$Sex=="male", ]
thirdM <- training[training$Pclass==3 & training$Sex=="male", ]
firstF <- training[training$Pclass==1 & training$Sex=="female", ]
secondF <- training[training$Pclass==2 & training$Sex=="female", ]
thirdF <- training[training$Pclass==3 & training$Sex=="female", ]

cherbourgM <- training[training$Embarked=="C" & training$Sex=="male", ]
queenstownM <- training[training$Embarked=="Q" & training$Sex=="male", ]
southamptonM <- training[training$Embarked=="S" & training$Sex=="male", ]
cherbourgF <- training[training$Embarked=="C" & training$Sex=="female", ]
queenstownF <- training[training$Embarked=="Q" & training$Sex=="female", ]
southamptonF <- training[training$Embarked=="S" & training$Sex=="female", ]

fm <- sum(firstM$Survived==1)/nrow(firstM)
sm <- sum(secondM$Survived==1)/nrow(secondM)
tm <- sum(thirdM$Survived==1)/nrow(thirdM)
ff <- sum(firstF$Survived==1)/nrow(firstF)
sf <- sum(secondF$Survived==1)/nrow(secondF)
tf <- sum(thirdF$Survived==1)/nrow(thirdF)

cm <- sum(cherbourgM$Survived==1)/nrow(cherbourgM)
qm <- sum(queenstownM$Survived==1)/nrow(queenstownM)
som <- sum(southamptonM$Survived==1)/nrow(southamptonM)
cf <- sum(cherbourgF$Survived==1)/nrow(cherbourgF)
qf <- sum(queenstownF$Survived==1)/nrow(queenstownF)
sof <- sum(southamptonF$Survived==1)/nrow(southamptonF)

classPort <- data.frame(
                male=c(fm,sm,tm,cm,qm,som),
                female=c(ff,sf,tf,cf,qf,sof)
        )
classPort <- cbind(CP=c('C1','C2','C3','PChe','PQuw','PSou'),classPort)












modFit <- randomForest(Survived~ ., data=training, importance=TRUE, proximity=TRUE)
# Displays the importance of each predictor.
importance(modFit)
# Tests on validation set 1.
prediction <- predict(modFit,testing[,-1])
confusionMatrix(testing$Survived, prediction)
