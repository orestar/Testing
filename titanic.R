library(caret); library(randomForest)

titanic <- read.csv("train.csv")

titanic$Embarked[titanic$Embarked==""] <- "S"

names <- as.character(titanic$Name)
lastname <- sapply(names,function(x) strsplit(x,",")[[1]][1])
titanic$Lastname <- as.vector(lastname)
title <- sapply(names,function(x) strsplit(x,",")[[1]][2])
title <- as.vector(title)
title <- sapply(title,function(x) strsplit(x," ")[[1]][2])
title <- as.vector(title)
titanic$Title <- as.factor(title)

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


t <- data.frame(Ticket=titanic$Ticket,Age=titanic$Age,Parch=titanic$Parch,
                SibSp=titanic$SibSp,Title=titanic$Title,FamSize=titanic$FamSize,
                Team=titanic$Team, Relation=titanic$Relation)
t <- t[order(t$Relation,t$Team),]
s<- t[order(t$Team,t$Relation),]
t[t$Team!="LoneWolf" & t$Relation==0,]
