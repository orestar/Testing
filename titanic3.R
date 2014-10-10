library(caret); library(randomForest)

# Loads and preprocesses the data
titanic <- read.csv("train.csv")


# Imputes the missing embarkation entries
titanic$Embarked[titanic$Embarked==""] <- "S"


### Imputes the ages by the implication from the titles.
# Extracts titles and lastnames
names <- as.character(titanic$Name)
lastname <- sapply(names,function(x) strsplit(x,",")[[1]][1])
titanic$Lastname <- as.vector(lastnames)
title <- sapply(names,function(x) strsplit(x,",")[[1]][2])
title <- as.vector(title)
title <- sapply(title,function(x) strsplit(x," ")[[1]][2])
title <- as.vector(title)
titanic$Title <- as.factor(title)

# Assuming all passengers with unusual titles were much attended and hence all 
# their info were not missing, it is okay to first impute the ages base on the 
# titles before reducing the titles.
titleNoAge <- unique(titanic$Title[is.na(titanic$Age)])
imputeMrAge <- median(titanic$Age[titanic$Title=="Mr."],na.rm=TRUE)
imputeMrsAge <- median(titanic$Age[titanic$Title=="Mrs."],na.rm=TRUE)
imputeMissAge <- median(titanic$Age[titanic$Title=="Miss."],na.rm=TRUE)
imputeMasterAge <- median(titanic$Age[titanic$Title=="Master."],na.rm=TRUE)
imputeDrAge <- median(titanic$Age[titanic$Title=="Dr."&titanic$Sex=="male"],na.rm=TRUE)
titanic$Age[titanic$Title=="Mr." & is.na(titanic$Age)] <- imputeMrAge
titanic$Age[titanic$Title=="Mrs." & is.na(titanic$Age)] <- imputeMrsAge
titanic$Age[titanic$Title=="Miss." & is.na(titanic$Age)] <- imputeMissAge
titanic$Age[titanic$Title=="Master." & is.na(titanic$Age)] <- imputeMasterAge
titanic$Age[titanic$Title=="Dr." & is.na(titanic$Age)] <- imputeDrAge


# Reduces titles 
titanic$Title[titanic$Title=="Don."] <- "Sir."
titanic$Title[titanic$Title=="Rev."] <- "Mr."
titanic$Title[titanic$Title=="Mme."] <- "Lady."
titanic$Title[titanic$Title=="Major."] <- "Sir."
titanic$Title[titanic$Title=="Mlle."] <- "Miss."
titanic$Title[titanic$Title=="Col."] <- "Sir."
titanic$Title[titanic$Title=="Capt."] <- "Sir."
titanic$Title[titanic$Title=="Ms."] <- "Miss."
titanic$Title[titanic$Sex=="male"  & 
                       !titanic$Title %in% c("Mr.","Master.","Sir.")] <- "Sir."
titanic$Title[titanic$Sex=="female"  & 
                       !titanic$Title %in% c("Mrs.","Miss.","Lady.")] <- "Lady."


# Locates Deck in possible cases
deck <- function(arg){                
        cab <- as.character(arg);
        result <- cab;
        cab <- strsplit(cab,"");
        if(length(cab[[1]])>1)
        {
                if(cab[[1]][2]==" ")
                        result <- cab[[1]][3]
                else
                        result <- cab[[1]][1]
        }
        result
}
titanic$Deck <- sapply(titanic$Cabin,deck)
titanic$Deck[which(titanic$Deck=="T")] <- ""
titanic$Deck[which(titanic$Deck=="")] <- "Z"


titanic$FamSize <- as.vector(titanic$SibSp+titanic$Parch+c(1))
titanic$FareAvg <- as.vector(as.numeric(titanic$Fare/titanic$FamSize))


# Groups families together
titanic$Team <- titanic$Lastname
titanic$Team[titanic$FamSize==1] <- "LoneWolf"
famTeam <- unique(titanic$Fare[which(titanic$Team != "LoneWolf")])
for(x in famTeam){
        indice <- which(titanic$Fare==x);
        for(i in indice){
                titanic$Team[i] <- paste(
                                         titanic$Lastname[i],"_",
                                         titanic$Fare[i],"_",
                                         titanic$Embarked[i],"_",
                                         titanic$Pclass[i],
                                         sep="") 
        }        
}



titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$SibSp <- as.factor(titanic$SibSp)
titanic$Parch <- as.factor(titanic$Parch)
titanic$FamSize <- as.factor(titanic$FamSize)
titanic$Deck <- as.factor(titanic$Deck)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Title <- as.factor(as.character(titanic$Title))
titanic$Team <- as.factor(titanic$Team)
















####################### Modeling & Predictions ##################################
titanic2 <- data.frame(
                        Survived=titanic$Survived,
                        Pclass=titanic$Pclass,
                        #Sex=titanic$Sex,
                        Age=titanic$Age,
                        #SibSp=titanic$SibSp,
                        Parch=titanic$Parch,
                        #Embarked=titanic$Embarked,
                        #Fare=titanic$Fare,
                        Lastname=titanic$Lastname,
                        Title=titanic$Title,
                        #FamSize=titanic$FamSize,
                        #FareAvg=titanic$FareAvg,
                        Deck=titanic$Deck,
                        Team=titanic$Team
                )

set.seed(314)
inTrain <- createDataPartition(y=titanic2$Survived, p=0.8, list=FALSE)
training <- titanic2[inTrain,]
testing <- titanic2[-inTrain,]

modFitRF <- randomForest(Survived~ ., data=training)
predictionRF <- predict(modFitRF,testing[,-1])
confusionMatrix(testing$Survived, predictionRF)

modFitLM <- lm(as.numeric(Survived)~ Pclass + Age + Parch + Lastname + Title + 
                        Deck + Team, data=training)
predictionLM <- predict(modFitLM,testing[,-1])
confusionMatrix(testing$Survived, predictionLM)

full.model <- lm(as.numeric(Survived)~ ., data=training)
reduced.model <- step(full.model, direction="backward")

#######################################################################
summary(titanic)
names(titanic)
names(titanic2)



plot(Survived~FamSize, data=titanic)
plot(Survived~SibSp, data=titanic)
plot(Survived~Parch, data=titanic)
plot(Age~Sex, data=titanic)
plot(FamSize~Sex, data=titanic)
plot(Sex~SibSp, data=titanic)
plot(FamSize~Pclass, data=titanic)

table(titanic$Pclass,titanic$FamSize)

table(titanic$Pclass,titanic$SibSp)

table(titanic$Pclass,titanic$Parch)

table(titanic$Sex,titanic$SibSp)

table(titanic$Sex,titanic$FamSize)

table(titanic$Survived,titanic$Parch)

table(titanic$Survived,titanic$SibSp)

table(titanic$Survived,titanic$FamSize)


head(titanic[order(titanic$Team,titanic$Age,titanic$Sex),c(5,6,14,18)],20)
titanic[order(titanic$Team,titanic$Age,titanic$Sex),]



