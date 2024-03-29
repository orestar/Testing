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
                titanic$Fare[i],"_",
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

head(titanic,50)
t <- data.frame(Ticket=titanic$Ticket,Age=titanic$Age,Parch=titanic$Parch,
                SibSp=titanic$SibSp,Title=titanic$Title,FamSize=titanic$FamSize,
                Team=titanic$Team, Relation=titanic$Relation)
t <- t[order(t$FamSize, t$Team),]
t[which(t$Relation==6 | t$Relation==10 | t$Relation==5 | t$Relation==9),]

tie <- function(FATHER=0, MOTHER=0, DAUGHTER1=0, SON1 =0, DAUGHTER2=0, SON2=0){
        1*FATHER + 2*MOTHER + 4*DAUGHTER1 + 8* SON1 + 16*DAUGHTER2 + 32*SON2
}


## Passengers traveled alone -- the majority.
# Rule 1:
titanic$Relation[titanic$FamSize==1 & titanic$Age>=18 &
                         (titanic$Title=="Mr." | titanic$Title=="Sir.")] <- tie(FATHER=1)

# Rule 2:
titanic$Relation[titanic$FamSize==1 & titanic$Title=="Mrs."] <- tie(MOTHER=1)

# Rule 3:
titanic$Relation[titanic$FamSize==1 & titanic$Age>=19 &
                         (titanic$Title=="Miss." | titanic$Title=="Lady.")] <- tie(MOTHER=1)

# Rule 4:
titanic$Relation[titanic$FamSize==1 & titanic$Age<18 & titanic$Title=="Mr."] <- tie(SON1 =1)

# Rule 5:
titanic$Relation[titanic$FamSize==1 & titanic$Title=="Master."] <- tie(SON1 =1)

# Rule 6:
titanic$Relation[titanic$FamSize==1 & titanic$Age<19 &
                         (titanic$Title=="Miss." | titanic$Title=="Lady.")] <- tie(DAUGHTER1=1)


family <- unique(titanic$Team[which(titanic$Team!="LoneWolf")])
for(fam in family)
{
        indices <- which(titanic$Team==fam)
        
        ## Second largest group
        if(titanic$FamSize[indices[1]]==2)
        {
                if(titanic$SibSp[indices[1]]==1)
                {
                        
                        # Married couple
                        if(titanic$Title[indices[1]]=="Mrs.")
                                titanic$Relation[indices] <- tie(FATHER=1, MOTHER=1)
                        
                        else if(titanic$Title[indices[1]]=="Miss.")
                        {
                                # Brother and Sister
                                if("Mr." %in% titanic$Title[indices] || "Master." %in% titanic$Title[indices])
                                        titanic$Relation[indices] <- tie(DAUGHTER1=1, SON1 =1)
                                
                                else # Sisters
                                        titanic$Relation[indices] <- tie(DAUGHTER1=1, DAUGHTER2=1)
                        }
                        else if (titanic$Title[indices[1]]=="Master.")
                                # Brothers
                                titanic$Relation[indices] <- tie(SON1 =1, SON2=1)
                        else # The first title in the indices is Mr., Sir. or Lady.
                        {
                                if(length(indices)>1)
                                {
                                        
                                        if(titanic$Title[indices[1]]==titanic$Title[indices[2]])
                                                titanic$Relation[indices] <- tie(SON1=1, SON2 =1)
                                                
                                        else
                                                titanic$Relation[indices] <- tie(FATHER=1, MOTHER=1)
                                }
                                else if(titanic$Age[indices[1]]>=18) # Married couple
                                        titanic$Relation[indices] <- tie(FATHER=1, MOTHER=1)
                                else # Brothers
                                        titanic$Relation[indices] <- tie(SON1=1, SON2 =1) 
                        }
                                
                                
                }
                
                if(titanic$Parch[indices[1]]==1)
                {
                        if(titanic$Title[indices[1]] == "Mrs.")
                        {
                                if(length(indices)>1)
                                {
                                        # Mother and daughter
                                        if(titanic$Title[indices[2]]=="Miss.")
                                                titanic$Relation[indices] <- tie(MOTHER=1, DAUGHTER1=1)
                                        else # Mother and son
                                                titanic$Relation[indices] <- tie(MOTHER=1, SON1=1)  
                                }
                                else # Mother and daughter
                                        titanic$Relation[indices] <- tie(MOTHER=1, DAUGHTER1=1) 
                                         
                        }
                        else if(titanic$Title[indices[1]] == "Miss.")
                        {
                                if(length(indices)>1)
                                {
                                        # Mother and daughter
                                        if(titanic$Title[indices[2]]=="Mrs.")
                                                titanic$Relation[indices] <- tie(MOTHER=1, DAUGHTER1=1)
                                        else # Father and daughter
                                                titanic$Relation[indices] <- tie(FATHER=1, DAUGHTER1=1)  
                                }
                                else # Mother and daughter
                                        titanic$Relation[indices] <- tie(MOTHER=1, DAUGHTER1=1) 
                                
                        }
                        else if(titanic$Title[indices[1]] == "Master." ||
                                        (titanic$Title[indices[1]] == "Mr." && 
                                                 titanic$Age[indices[1]] <= 18 ))
                        {
                                if(length(indices)>1)
                                {
                                        # Mother and son
                                        if(titanic$Title[indices[2]]=="Mrs.")
                                                titanic$Relation[indices] <- tie(MOTHER=1, SON1=1)
                                        else # Father and son
                                                titanic$Relation[indices] <- tie(FATHER=1, SON1=1)  
                                }
                                else # Mother and son
                                        titanic$Relation[indices] <- tie(MOTHER=1, SON1=1) 
                                
                        }
                        else if(titanic$Title[indices[1]] == "Mr." && 
                                                 titanic$Age[indices[1]] > 18 )
                        {
                                if(length(indices)>1)
                                {
                                        # Mother and son
                                        if(titanic$Title[indices[2]]=="Mrs.")
                                                titanic$Relation[indices] <- tie(MOTHER=1, SON1=1)
                                        else # Father and son
                                                titanic$Relation[indices] <- tie(FATHER=1, SON1=1)  
                                }
                                else # FAther and son
                                        titanic$Relation[indices] <- tie(FATHER=1, SON1=1) 
                                
                        }
                        else
                        {
                                if(length(indices)>1)
                                {
                                        # Father and daughter
                                        if(titanic$Title[indices[2]]=="Miss.")
                                                titanic$Relation[indices] <- tie(FATHER=1, DAUGHTER1=1)
                                        else # Father and son
                                                titanic$Relation[indices] <- tie(FATHER=1, SON1=1)  
                                }
                                else # Father and son
                                        titanic$Relation[indices] <- tie(FATHER=1, SON1=1) 
                                
                        }
                }
        }
        # Groups of more than two passengers. Most probably there were at least 
        # one parent and one child.
        else 
        {
                famDf <- data.frame(Title=titanic$Title[indices],
                                    Age=titanic$Age[indices],
                                    Parch=titanic$Parch[indices],
                                    SibSp=titanic$SibSp[indices],
                                    FamSize=titanic$FamSize[indices]);
                famDf <- famDf[order(famDf$Age),];
                famDf <- famDf[rev(order(famDf$Title)),];
                print(famDf)
                if(sum(famDf$Parch==0)) # Only siblings and spouses traveled together
                {
                        sons <- 0; daughters <- 0;
                        for(i in 1:nrow(famDf)){
                                if(sons<2 && 
                                           (famDf$Title[i]=="Mr." || 
                                                    famDf$Title[i]=="Master." || 
                                                    famDf$Title[i]=="Sir."))
                                        sons <- sons+1
                                if(daughters<2 && 
                                           (famDf$Title[i]=="Miss." || 
                                                    famDf$Title[i]=="Lady." ||
                                                    famDf$Title[i]=="Mrs."))
                                        daughters <- daughters+1
                        }
                        d1 <-0; d2 <-0; s1 <- 0; s2 <- 0;
                        # Number of family memebers not in the training set
                        notSeen <- famDf$FamSize[1]- nrow(famDf);
                        if(notSeen==0) # Everyone was in the training set
                        {
                                if(daughters==1) {d1=1; d2=0}
                                if(daughters==2) {d1=1; d2=1}
                                if(sons==1) {s1=1; s2=0}
                                if(sons==2) {s1=1; s2=1}
                        }
                        else
                        {
                                daughters <- daughters + floor(notSeen/2);
                                sons <- sons + ceiling(notSeen/2);
                                if(daughters==1) {d1=1; d2=0}
                                if(daughters>=2) {d1=1; d2=1}
                                if(sons==1) {s1=1; s2=0}
                                if(sons>=2) {s1=1; s2=1}        
                        }                        
                        titanic$Relation[indices] <- tie(DAUGHTER1=d1, SON1=s1,
                                                         DAUGHTER2=d2,SON2=s2) 
                }
        }
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



