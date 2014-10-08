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

plot(Pclass~Embarked, data=training)

male <- which(training$Sex=="male")
female <- which(training$Sex=="female")
first <- which(training$Pclass==1)
second <- which(training$Pclass==2)
third <- which(training$Pclass==3)
cherbourg <- which(training$Embarked=="C")
queenstown <- which(training$Embarked=="Q")
southampton <- which(training$Embarked=="S")


firstM <- training[intersect(male,first), ]
secondM <- training[intersect(male,second), ]
thirdM <- training[intersect(male,third), ]
firstF <- training[intersect(female,first), ]
secondF <- training[intersect(female,second), ]
thirdF <- training[intersect(female,third), ]

cherbourgM <- training[intersect(male,cherbourg), ]
queenstownM <- training[intersect(male,queenstown), ]
southamptonM <- training[intersect(male,southampton), ]
cherbourgF <- training[intersect(female,cherbourg), ]
queenstownF <- training[intersect(female,queenstown), ]
southamptonF <- training[intersect(female,southampton), ]

m_1_ch <- training[intersect(intersect(male,first),cherbourg), ]
m_1_qu <- training[intersect(intersect(male,first),queenstown), ]
m_1_so <- training[intersect(intersect(male,first),southampton), ]
m_2_ch <- training[intersect(intersect(male,second),cherbourg), ]
m_2_qu <- training[intersect(intersect(male,second),queenstown), ]
m_2_so <- training[intersect(intersect(male,second),southampton), ]
m_3_ch <- training[intersect(intersect(male,third),cherbourg), ]
m_3_qu <- training[intersect(intersect(male,third),queenstown), ]
m_3_so <- training[intersect(intersect(male,third),southampton), ]

f_1_ch <- training[intersect(intersect(female,first),cherbourg), ]
f_1_qu <- training[intersect(intersect(female,first),queenstown), ]
f_1_so <- training[intersect(intersect(female,first),southampton), ]
f_2_ch <- training[intersect(intersect(female,second),cherbourg), ]
f_2_qu <- training[intersect(intersect(female,second),queenstown), ]
f_2_so <- training[intersect(intersect(female,second),southampton), ]
f_3_ch <- training[intersect(intersect(female,third),cherbourg), ]
f_3_qu <- training[intersect(intersect(female,third),queenstown), ]
f_3_so <- training[intersect(intersect(female,third),southampton), ]



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

m_1_chR <- sum(m_1_ch$Survived==1)/nrow(m_1_ch)
m_1_quR <- sum(m_1_qu$Survived==1)/nrow(m_1_qu)
m_1_soR <- sum(m_1_so$Survived==1)/nrow(m_1_so)
m_2_chR <- sum(m_2_ch$Survived==1)/nrow(m_2_ch)
m_2_quR <- sum(m_2_qu$Survived==1)/nrow(m_2_qu)
m_2_soR <- sum(m_2_so$Survived==1)/nrow(m_2_so)
m_3_chR <- sum(m_3_ch$Survived==1)/nrow(m_3_ch)
m_3_quR <- sum(m_3_qu$Survived==1)/nrow(m_3_qu)
m_3_soR <- sum(m_3_so$Survived==1)/nrow(m_3_so)

f_1_chR <- sum(f_1_ch$Survived==1)/nrow(f_1_ch)
f_1_quR <- sum(f_1_qu$Survived==1)/nrow(f_1_qu)
f_1_soR <- sum(f_1_so$Survived==1)/nrow(f_1_so)
f_2_chR <- sum(f_2_ch$Survived==1)/nrow(f_2_ch)
f_2_quR <- sum(f_2_qu$Survived==1)/nrow(f_2_qu)
f_2_soR <- sum(f_2_so$Survived==1)/nrow(f_2_so)
f_3_chR <- sum(f_3_ch$Survived==1)/nrow(f_3_ch)
f_3_quR <- sum(f_3_qu$Survived==1)/nrow(f_3_qu)
f_3_soR <- sum(f_3_so$Survived==1)/nrow(f_3_so)




classPort <- data.frame(
                male=c(fm,sm,tm,cm,qm,som,m_1_chR,m_1_quR,m_1_soR,m_2_chR,m_2_quR,
                       m_2_soR,m_3_chR,m_3_quR,m_3_soR),
                female=c(ff,sf,tf,cf,qf,sof,f_1_chR,f_1_quR,f_1_soR,f_2_chR,f_2_quR,
                         f_2_soR,f_3_chR,f_3_quR,f_3_soR)
        )
classPort <- cbind(CP=c('1','2','3','Ch','Qu','So','1_ch','1_qu','1_so',
                        '2_ch','2_qu','2_so','3_ch','3_qu','3_so'),classPort)












modFit <- randomForest(Survived~ ., data=training, importance=TRUE, proximity=TRUE)
# Displays the importance of each predictor.
importance(modFit)
# Tests on validation set 1.
prediction <- predict(modFit,testing[,-1])
confusionMatrix(testing$Survived, prediction)
