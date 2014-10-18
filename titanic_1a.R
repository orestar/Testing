# cforest with Inputs:  Pclass, AgeGroup, SibSp, Parch, Title, FamSize, 
# FareAvgGroup, Side, Team yields Accuracy : 0.8701 at local test set and
# 0.82775 at Kaggle.


library(caret); library(randomForest); library(reshape2); library(stringr)

test <- read.csv("test.csv")
test <- cbind(PassengerId=test[,1], Survived=c(NA),test[,2:length(test)])
titanic <- read.csv("train.csv")


# Combines the training set and the actual test set so that
# the test set will be processed and ready to undergo the prediction.
titanic <- rbind(titanic,test)


# Imputes the missing Embarked entries(2) by the mode.
titanic$Embarked[titanic$Embarked==""] <- "S"
titanic$Embarked <- as.factor(as.character(titanic$Embarked))


# Imputes the missing Fare entry by the average of the class.
imputedFare <- mean(titanic$Fare[titanic$Pclass==3],na.rm=TRUE)
titanic$Fare[is.na(titanic$Fare)] <- imputedFare 


# Extracts the titles and surnames for later use.
{
names <- as.character(titanic$Name)
lastname <- sapply(names,function(x) strsplit(x,",")[[1]][1])
titanic$Lastname <- as.vector(lastname)
title <- sapply(names,function(x) strsplit(x,",")[[1]][2])
title <- as.vector(title)
title <- sapply(title,function(x) strsplit(x," ")[[1]][2])
title <- as.vector(title)
titanic$Title <- as.factor(title)
}

# Simplifies the titles.
{
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
titanic$Title[titanic$Title=="Don."] <- "Sir."
titanic$Title[titanic$Title=="Rev."] <- "Mr."
titanic$Title[titanic$Title=="Mme."] <- "Lady."
titanic$Title[titanic$Title=="Major."] <- "Sir."
titanic$Title[titanic$Title=="Mlle."] <- "Miss."
titanic$Title[titanic$Title=="Col."] <- "Sir."
titanic$Title[titanic$Title=="Capt."] <- "Sir."
titanic$Title[titanic$Title=="Ms."] <- "Miss."
titanic$Title[titanic$Title=="Dr."&titanic$Sex=="male"] <- "Mr."
titanic$Title[titanic$Title=="Dr."&titanic$Sex=="female"] <- "Mrs."
titanic$Title[titanic$Sex=="male"  & 
                      !titanic$Title %in% c("Mr.","Master.","Sir.")] <- "Sir."
titanic$Title[titanic$Sex=="female"  & 
                      !titanic$Title %in% c("Mrs.","Miss.","Lady.")] <- "Lady."
# Drops the levels that are no longer in use.
titanic$Title <- as.factor(as.character(titanic$Title))
}

# Breaks Age into various categories.
titanic$AgeGroup <- cut(titanic$Age, 
                        breaks=c(0,10,15,20,25,30,40,50,60,90))


# Imputes missing Age entries.
{
titanic_AllAge <- titanic[!is.na(titanic$Age),]
titleNoAge <- unique(titanic$Title[which(is.na(titanic$Age))])
age_cut <- sort(unique(titanic_AllAge$AgeGroup))
{
age_group_mr <- sapply(1:length(age_cut), 
                       function(i){
                                sum(titanic_AllAge$Title=="Mr." & 
                                        titanic_AllAge$AgeGroup==age_cut[i])/sum(titanic_AllAge$Title=="Mr.")
                       }
                )
noAge <- sum(titanic$Title=="Mr." & is.na(titanic$Age))
n5 <- round(age_group_mr[1]*noAge); n12 <- round(age_group_mr[2]*noAge); n17 <- round(age_group_mr[3]*noAge);
n22 <- round(age_group_mr[4]*noAge); n27 <- round(age_group_mr[5]*noAge); n35 <- round(age_group_mr[6]*noAge);
n45 <- round(age_group_mr[7]*noAge); n55 <- round(age_group_mr[8]*noAge); n75 <- round(age_group_mr[9]*noAge);
noAge;n5;n12;n17;n22;n27;n35;n45;n55;n75
noAge_indices <- which(titanic$Title=="Mr." & is.na(titanic$Age))
titanic$Age[noAge_indices[1:n12]] <- 12;
titanic$Age[noAge_indices[(n12+1):(n12+n17)]] <- 17;
titanic$Age[noAge_indices[(n12+n17+1):(n12+n17+n22)]] <- 22;
titanic$Age[noAge_indices[(n12+n17+n22+1):(n12+n17+n22+n27)]] <- 27;
titanic$Age[noAge_indices[(n12+n17+n22+n27+1):(n12+n17+n22+n27+n35)]] <- 35;
titanic$Age[noAge_indices[(n12+n17+n22+n27+n35+1):(n12+n17+n22+n27+n35+n45)]] <- 45;
titanic$Age[noAge_indices[(n12+n17+n22+n27+n35+n45+1):(n12+n17+n22+n27+n35+n45+n55)]] <- 55;
titanic$Age[noAge_indices[(n12+n17+n22+n27+n35+n45+n55+1):length(noAge_indices)]] <- 75;
}
{
age_group_mrs <- sapply(1:length(age_cut), 
                       function(i){
                               sum(titanic_AllAge$Title=="Mrs." & 
                                           titanic_AllAge$AgeGroup==age_cut[i])/sum(titanic_AllAge$Title=="Mrs.")
                       }
                )
noAge <- sum(titanic$Title=="Mrs." & is.na(titanic$Age))
n5 <- round(age_group_mrs[1]*noAge); n12 <- round(age_group_mrs[2]*noAge); n17 <- round(age_group_mrs[3]*noAge);
n22 <- round(age_group_mrs[4]*noAge); n27 <- round(age_group_mrs[5]*noAge); n35 <- round(age_group_mrs[6]*noAge);
n45 <- round(age_group_mrs[7]*noAge); n55 <- round(age_group_mrs[8]*noAge); n75 <- round(age_group_mrs[9]*noAge);
noAge;n5;n12;n17;n22;n27;n35;n45;n55;n75

noAge_indices <- which(titanic$Title=="Mrs." & is.na(titanic$Age))
titanic$Age[noAge_indices[1:n17]] <- 17;
titanic$Age[noAge_indices[(n12+n17+1):(n12+n17+n22)]] <- 22;
titanic$Age[noAge_indices[(n12+n17+n22+1):(n12+n17+n22+n27)]] <- 27;
titanic$Age[noAge_indices[(n12+n17+n22+n27+1):(n12+n17+n22+n27+n35)]] <- 35;
titanic$Age[noAge_indices[(n12+n17+n22+n27+n35+1):(n12+n17+n22+n27+n35+n45)]] <- 45;
titanic$Age[noAge_indices[(n12+n17+n22+n27+n35+n45+1):(n12+n17+n22+n27+n35+n45+n55)]] <- 55;
titanic$Age[noAge_indices[(n12+n17+n22+n27+n35+n45+n55+1):length(noAge_indices)]] <- 75;
}
{
age_group_master <- sapply(1:length(age_cut), 
                       function(i){
                               sum(titanic_AllAge$Title=="Master." & 
                                           titanic_AllAge$AgeGroup==age_cut[i])/sum(titanic_AllAge$Title=="Master.")
                       }
)
noAge <- sum(titanic$Title=="Master." & is.na(titanic$Age))
n5 <- round(age_group_master[1]*noAge); n12 <- round(age_group_master[2]*noAge)
noAge;n5;n12;
noAge_indices <- which(titanic$Title=="Master." & is.na(titanic$Age))
titanic$Age[noAge_indices[1:n5]] <- 5;
titanic$Age[noAge_indices[(n5+1):length(noAge_indices)]] <- 12
}
{
age_group_miss <- sapply(1:length(age_cut), 
                       function(i){
                               sum(titanic_AllAge$Title=="Miss." & 
                                           titanic_AllAge$AgeGroup==age_cut[i])/sum(titanic_AllAge$Title=="Miss.")
                       }
        )
noAge <- sum(titanic$Title=="Miss." & is.na(titanic$Age))
n5 <- round(age_group_miss[1]*noAge); n12 <- round(age_group_miss[2]*noAge); n17 <- round(age_group_miss[3]*noAge);
n22 <- round(age_group_miss[4]*noAge); n27 <- round(age_group_miss[5]*noAge); n35 <- round(age_group_miss[6]*noAge);
n45 <- round(age_group_miss[7]*noAge); n55 <- round(age_group_miss[8]*noAge); n75 <- round(age_group_miss[9]*noAge);
noAge;n5;n12;n17;n22;n27;n35;n45;n55;n75
noAge_indices <- which(titanic$Title=="Miss." & is.na(titanic$Age))
titanic$Age[noAge_indices[1:n5]] <- 5;
titanic$Age[noAge_indices[(n5+1):(n5+n12)]] <- 12;
titanic$Age[noAge_indices[(n5+n12+1):(n5+n12+n17)]] <- 17;
titanic$Age[noAge_indices[(n5+n12+n17+1):(n5+n12+n17+n22)]] <- 22;
titanic$Age[noAge_indices[(n5+n12+n17+n22+1):(n5+n12+n17+n22+n27)]] <- 27;
titanic$Age[noAge_indices[(n5+n12+n17+n22+n27+1):(n5+n12+n17+n22+n27+n35)]] <- 35;
titanic$Age[noAge_indices[(n5+n12+n17+n22+n27+n35+1):(n5+n12+n17+n22+n27+n35+n45)]] <- 45
}
{
imputeSirAge <- median(titanic$Age[titanic$Title=="Sir."],na.rm=TRUE)
titanic$Age[titanic$Title=="Sir." & is.na(titanic$Age)] <- imputeSirAge
}
titanic$AgeGroup <- cut(titanic$Age, 
                        breaks=c(0,10,15,20,25,30,40,50,60,90))
}

# Extracts the deck on which a passenger located.
{
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
}


# Extracts the side of the ship on which a passenger located.
{
lastDigit <- str_sub(titanic$Cabin,-1)
side <- function(s){
        result <- "unknown";
        x <- as.numeric(s);
        if(!is.na(x))
                result <- ifelse(x==1,"starboard","port") 
                #        result <- "starboard" 
                #else result <- "port"              
        result
}
titanic$Side <- sapply(1:length(lastDigit), function(i) side(lastDigit[i]))
}


# Calculates the family size of each passenger,
titanic$FamSize <- as.vector(titanic$SibSp+titanic$Parch+c(1))


# Groups and labels each family of size 2 or more and 
# labels the passengers traveled alone as "LoneWolf".
{
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
}


# derives relations within each family of size 2 or more.
{
titanic$Relation <- c(0)
tie <- function(FATHER=0, MOTHER=0, DAUGHTER1=0, SON1 =0, DAUGHTER2=0, SON2=0){
        1*FATHER + 2*MOTHER + 4*DAUGHTER1 + 8* SON1 + 16*DAUGHTER2 + 32*SON2
}

titanic$Relation[titanic$FamSize==1 & titanic$Age>=18 &
                         (titanic$Title=="Mr." | titanic$Title=="Sir.")] <- tie(FATHER=1)
titanic$Relation[titanic$FamSize==1 & titanic$Title=="Mrs."] <- tie(MOTHER=1)
titanic$Relation[titanic$FamSize==1 & titanic$Age>=19 &
                         (titanic$Title=="Miss." | titanic$Title=="Lady.")] <- tie(MOTHER=1)
titanic$Relation[titanic$FamSize==1 & titanic$Age<18 & titanic$Title=="Mr."] <- tie(SON1 =1)
titanic$Relation[titanic$FamSize==1 & titanic$Title=="Master."] <- tie(SON1 =1)
titanic$Relation[titanic$FamSize==1 & titanic$Age<19 &
                         (titanic$Title=="Miss." | titanic$Title=="Lady.")] <- tie(DAUGHTER1=1)

family <- unique(titanic$Team[which(titanic$Team!="LoneWolf")])
for(fam in family)
{
        indices <- which(titanic$Team==fam);
        famDf <- data.frame(Title=titanic$Title[indices],
                            Age=titanic$Age[indices],
                            Parch=titanic$Parch[indices],
                            SibSp=titanic$SibSp[indices],
                            FamSize=titanic$FamSize[indices],
                            Relation=titanic$Relation[indices]);
        famDf <- famDf[order(famDf$Age),];
        famDf <- famDf[rev(order(famDf$Title)),];
        #print(famDf)
        fa<-0; ma<-0; d1<-0; s1<-0; d2<-0; s2<-0;
        daughters <- 0;  sons <- 0;
        notSeen <- 0;
        
        
        # Groups of two that are either married couples or siblings
        if(famDf$SibSp[1]==1 && famDf$FamSize[1]==2)
        {
                # Married couple
                if(famDf$Title[1]=="Mrs."){
                        if( length(indices==1)||
                                    (famDf$Title[2]=="Mr." && famDf$Age[2]>= famDf$Age[1]-8) )
                        {fa<-1;ma<-1}                
                }
                # Married couple
                else if(famDf$Title[1]=="Mr." && famDf$Age[1]>=18 && length(indices)==1)
                {fa<-1;ma<-1}
                else if(famDf$Title[1]=="Miss.")
                {   
                        # Brother and Sister
                        if("Master." %in% famDf$Title)
                        {d1<-1;s1<-1}  
                        # Two sisters
                        else    {d1<-1;d2<-1}
                }
                # Two brothers
                else if (famDf$Title[1]=="Master.")     
                {s1<-1;s2<-1}
                # Covers Sir., Lady., and Mr.s having length(indices)>1
                else # The first title in the indices is Mr., Sir. or Lady.
                {
                        if(famDf$Title[1]=="Mr." && length(indices)>1){
                                if(famDf$Title[2]=="Miss.")     {d1<-1; s1<-1}
                                else    {s1<-1; s2<-1}
                        }
                        else if(famDf$Title[1]=="Mr." && famDf$Age[1]<=17)  
                                s1 <- 1    
                        else    {fa<-1;ma<-1}
                }     
        }
        
        # Groups of three or more siblings (possibly with their spouses)
        # with no parents
        else if(famDf$FamSize[1]>2 && sum(famDf$Parch)==0)
        {
                for(i in nrow(famDf):1){
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
                
                # Number of family memebers not in the training set
                notSeen <- famDf$FamSize[1]- nrow(famDf);
        }
        
        # Groups involving at least one parent
        else
        {
                children <- 0;
                firstChild <- 2;
                
                # Mother in the training set
                if(famDf$Title[1]=="Mrs."){
                        ma <- 1;
                        if(famDf$SibSp[1]>=1)   fa <-1
                        children <- famDf$Parch[1];
                        if(length(indices)>1){
                                if(famDf$Title[2]=="Mr." && 
                                           famDf$Age[2]>= famDf$Age[1]-8)
                                        firstChild <- 3
                                if(length(indices)>=firstChild){
                                        for(i in nrow(famDf):firstChild){
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
                                }
                        }
                        # Number of children not in the training set
                        notSeen <- children-(nrow(famDf)-firstChild+1);
                }
                
                else if(famDf$Title[1]=="Miss." || famDf$Title[1]=="Master.")
                {                        
                        if(famDf$Parch[1]==1) ma <- 1;
                        if(famDf$Parch[1]==2) {ma <- 1;fa <-1}
                        children <- famDf$SibSp[1]+1;
                        firstChild <- 1;
                        if(length(indices)>=firstChild){
                                for(i in nrow(famDf):firstChild){
                                        if(sons<2 && famDf$Title[i]=="Master.")
                                                sons <- sons+1
                                        if(daughters<2 && famDf$Title[i]=="Miss.")
                                                daughters <- daughters+1
                                }
                        }
                        # Number of children not in the training set
                        notSeen <- children-(nrow(famDf)-firstChild+1);
                }
                
                else if(famDf$Title[nrow(famDf)]=="Miss." || famDf$Title[nrow(famDf)]=="Master.")
                {
                        children <- famDf$SibSp[nrow(famDf)]+1;
                        seenChildren <- 0;
                        
                        firstChild <- 1;
                        
                        if(famDf$SibSp[nrow(famDf)]!=famDf$SibSp[1] ||
                                   famDf$Parch[nrow(famDf)]!=famDf$Parch[1] ||
                                   famDf$Age[nrow(famDf)]<= famDf$Age[1]-17)
                        {
                                fa <- 1;
                                if(famDf$Parch[nrow(famDf)]==2) 
                                        ma <-1
                                firstChild <- 2;
                        }
                        else
                        {
                                if(famDf$Parch[1]==1) ma <- 1;
                                if(famDf$Parch[1]==2) 
                                        {ma <- 1; fa <-1}
                        }      
                        
                        for(i in nrow(famDf):firstChild)
                        {
                                seenChildren <- seenChildren+1;
                                if(sons<2 && 
                                           (famDf$Title[i]=="Master."||
                                                    famDf$Title[i]=="Mr."))
                                        sons <- sons+1
                                if(daughters<2 && famDf$Title[i]=="Miss.")
                                        daughters <- daughters+1
                        }
                        
                        
                        # Number of children not in the training set
                        notSeen <- children-seenChildren;
                }
                
                # The Mr.'s
                else
                {                       
                        if(length(indices)==1){
                                
                                if(famDf$SibSp[1]>1){
                                        if(famDf$Parch==1) ma <- 1
                                        if(famDf$Parch==2){
                                                ma <- 1; fa <- 1
                                        }
                                        sons <- sons +1;
                                        notSeen <- famDf$SibSp[1]
                                }
                                else{
                                        if(famDf$Age[1]>=19){
                                                if(famDf$Parch[1]!=0){
                                                        fa <- 1;
                                                        if(famDf$SibSp[1]==1)  ma <- 1
                                                        notSeen <- famDf$Parch[1]  
                                                }        
                                        }
                                        else{
                                                if(famDf$Parch==1) ma <- 1
                                                if(famDf$Parch==2){
                                                        ma <- 1; fa <- 1
                                                }
                                                sons <- sons +1;
                                                notSeen <- famDf$SibSp[1] 
                                        }
                                }
                        }
                        else{
                                firstChild <- 1;
                                if(famDf$SibSp[nrow(famDf)]!=famDf$SibSp[1] ||
                                           famDf$Parch[nrow(famDf)]!=famDf$Parch[1] ||
                                           famDf$Age[nrow(famDf)] <= famDf$Age[1]-17)
                                {
                                        firstChild <- 2;
                                        fa <- 1;
                                        if(famDf$SibSp[1]==1)  ma <- 1
                                        sons <- nrow(famDf)-1;
                                        notSeen <- famDf$Parch[1]-sons
                                }
                                else
                                {
                                        if(famDf$Parch[1]==1)   ma <- 1
                                        if(famDf$Parch[1]==2) {fa <- 1; ma <- 1}
                                        sons <- nrow(famDf);
                                        notSeen <- famDf$SibSp[1]+1-sons
                                }
                        }
                }
        }
        
        # Calculates the relation code
        if(notSeen!=0) # Someone in the family was not in the training set
        {
                daughters <- daughters + floor(notSeen/2);
                sons <- sons + ceiling(notSeen/2);
        }
        if(daughters==1) {d1=1; d2=0}
        if(daughters>=2) {d1=1; d2=1}
        if(sons==1) {s1=1; s2=0}
        if(sons>=2) {s1=1; s2=1}
        titanic$Relation[indices] <- tie(FATHER=fa, MOTHER=ma,
                                         DAUGHTER1=d1, SON1=s1,
                                         DAUGHTER2=d2,SON2=s2)
}
}


# Calculates the average fare each passenger within a family paid.  
titanic$FareAvg <- as.vector(as.numeric(titanic$Fare/titanic$FamSize))


# Breaks FareAvg into various categories
p3 <- 0:9; p2 <- 3*4:15; p1 <- c(60,75,100,200,300,600)
titanic$FareAvgGroup <- cut(titanic$FareAvg, breaks=c(p3,p2,p1))



# Categorizes the variables
{
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$SibSp <- as.factor(titanic$SibSp)
titanic$Parch <- as.factor(titanic$Parch)
titanic$FamSize <- as.factor(titanic$FamSize)
titanic$Deck <- as.factor(titanic$Deck)
titanic$Side <- as.factor(titanic$Side)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Title <- as.factor(as.character(titanic$Title))
titanic$Team <- as.factor(titanic$Team)
titanic$Relation <- as.factor(titanic$Relation)
}

names(titanic)
####################### Modeling & Predictions ##################################
titanic2 <- data.frame(
        Survived=titanic$Survived,
        Pclass=titanic$Pclass,
        #Sex=titanic$Sex,
        #Age=titanic$Age,
        AgeGroup=titanic$AgeGroup,
        SibSp=titanic$SibSp,
        Parch=titanic$Parch,
        #Embarked=titanic$Embarked,
        #Fare=titanic$Fare,
        #Lastname=titanic$Lastname,
        Title=titanic$Title,
        FamSize=titanic$FamSize,
        #FareAvg=titanic$FareAvg,
        FareAvgGroup=titanic$FareAvgGroup,
        #Deck=titanic$Deck,
        Side=titanic$Side,
        Team=titanic$Team
        #Relation=titanic$Relation    
)

theTestSet <- titanic2[is.na(titanic2$Survived),]
titanic2 <- titanic2[!is.na(titanic$Survived),]

set.seed(314)
inTrain <- createDataPartition(y=titanic2$Survived, p=0.8, list=FALSE)
training <- titanic2[inTrain,]
testing <- titanic2[-inTrain,]


library(party)
modFitRF <- cforest(Survived~ ., data=training, )
predictionRF <- predict(modFitRF,testing[,-1], OOB=TRUE, type = "response")
confusionMatrix(testing$Survived, predictionRF)



predictionRF <- predict(modFitRF,theTestSet, OOB=TRUE, type = "response")
testDf <- data.frame(PassengerId=test$PassengerId,
                     Survived=as.vector(predictionRF))
write.csv(testDf, "titanicML.csv", row.names = FALSE)
