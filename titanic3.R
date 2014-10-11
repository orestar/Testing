library(caret); library(randomForest)

# Loads and preprocesses the data
titanic <- read.csv("train.csv")


# Imputes the missing embarkation entries
titanic$Embarked[titanic$Embarked==""] <- "S"


### Imputes the ages by the implication from the titles.
# Extracts titles and lastnames (for later use)
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
imputeDrAge <- median(titanic$Age[titanic$Title=="Dr."],na.rm=TRUE)
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
titanic$Team=""
titanic$Team[titanic$FamSize==1] <- "LoneWolf"
famTeam <- which(!titanic$Team=="LoneWolf")
for(i in famTeam){
        titanic$Team[i] <- paste(
                titanic$Lastname[i],"_",
                titanic$Ticket[i],"_",
                titanic$Embarked[i],"_",
                titanic$Pclass[i],"_",
                titanic$FamSize[i],
                sep="")        
}


################ Derives relations with the families ###########################
# Structure: Binary tuples (Father, Mother, Daughter, Son1, Son2)
# Rules:
# 1. A Mr./Sir. of age >= 18 traveled alone is Father.
# 2. A Mrs. traveled alone is Mother.
# 3. A Miss./Lady. of age >= 19 is Mother.
# 4. A Mr. of age < 18 traveled alone is Son1.
# 5. A Master. traveled alone is Son1.
# 6. A Miss./Lady. of age < 19 is Daughter.
# 7. A Mr. and a Mrs. with FamSize=2 are Father and Mother.
# 8. If there was n Mrs. in a team and the largest difference in age is less 
#    than 18, the members of the team are siblings.
# 9. If the largest difference in age is at least 18 and Parch>0, the oldest 
#    member is a parent to the youngest one. 
###
FATHER=0; MOTHER=0; DAUGHTER1=0; SON1 =0; DAUGHTER2=0; SON2=0

head(titanic,50)
t <- data.frame(Ticket=titanic$Ticket,Age=titanic$Age,Parch=titanic$Parch,SibSp=titanic$SibSp,Title=titanic$Title,FamSize=titanic$FamSize,Team=titanic$Team)
t <- t[order(t$Team),]
t[which(t$Relation==4),]


# Rule 1:
FATHER=1; MOTHER=0; DAUGHTER1=0; SON1 =0; DAUGHTER2=0; SON2=0
tie <-1*FATHER + 2*MOTHER + 4*DAUGHTER1 + 8* SON1 + 16*DAUGHTER2 + 32*SON2
titanic$Relation[titanic$FamSize==1 & titanic$Age>=18 &
                         (titanic$Title=="Mr." | titanic$Title=="Sir.")] <- tie

# Rule 2:
FATHER=0; MOTHER=1; DAUGHTER1=0; SON1 =0; DAUGHTER2=0; SON2=0
tie <-1*FATHER + 2*MOTHER + 4*DAUGHTER1 + 8* SON1 + 16*DAUGHTER2 + 32*SON2
titanic$Relation[titanic$FamSize==1 & titanic$Title=="Mrs."] <- tie


# Rule 3:
FATHER=0; MOTHER=1; DAUGHTER1=0; SON1 =0; DAUGHTER2=0; SON2=0
tie <-1*FATHER + 2*MOTHER + 4*DAUGHTER1 + 8* SON1 + 16*DAUGHTER2 + 32*SON2
titanic$Relation[titanic$FamSize==1 & titanic$Age>=19 &
                         (titanic$Title=="Miss." | titanic$Title=="Lady.")] <- tie

# Rule 4:
FATHER=0; MOTHER=0; DAUGHTER1=0; SON1 =1; DAUGHTER2=0; SON2=0
tie <-1*FATHER + 2*MOTHER + 4*DAUGHTER1 + 8* SON1 + 16*DAUGHTER2 + 32*SON2
titanic$Relation[titanic$FamSize==1 & titanic$Age<18 & titanic$Title=="Mr."] <- tie

# Rule 5:
FATHER=0; MOTHER=0; DAUGHTER1=0; SON1 =1; DAUGHTER2=0; SON2=0
tie <-1*FATHER + 2*MOTHER + 4*DAUGHTER1 + 8* SON1 + 16*DAUGHTER2 + 32*SON2
titanic$Relation[titanic$FamSize==1 & titanic$Title=="Master."] <- tie

# Rule 6:
FATHER=0; MOTHER=0; DAUGHTER1=1; SON1 =0; DAUGHTER2=0; SON2=0
tie <-1*FATHER + 2*MOTHER + 4*DAUGHTER1 + 8* SON1 + 16*DAUGHTER2 + 32*SON2
titanic$Relation[titanic$FamSize==1 & titanic$Age<19 &
                         (titanic$Title=="Miss." | titanic$Title=="Lady.")] <- tie

family <- unique(titanic$Team[which(titanic$Team!="LoneWolf")])
for(fam in family){
        indices <- which(titanic$Team==fam)
        print(paste("(",indices,"),",fam,sep=""))
}



































# Categorizes the variables
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



