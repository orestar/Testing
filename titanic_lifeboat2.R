####################  Loading and Processing  ################################
library(caret); library(randomForest);library(stringr)

# Loads and combines the data.
{
        # Data on Lifeboats
        lifeBoatData <- read.csv("http://mysafeinfo.com/api/data?list=titanic&format=csv")
        
        test <- read.csv("test.csv")
        test <- cbind(PassengerId=test[,1], Survived=c(NA),test[,2:length(test)])
        titanic <- read.csv("train.csv")
        
        # Combines the training set and the actual test set so that
        # the test set will be processed and ready to undergo the prediction.
        titanic <- rbind(titanic,test)       
}
# Removes entries from the lifeboat data which are not relevant for the prediction.
{
lifeBoatData <- lifeBoatData[!lifeBoatData$Board.Point=="Belfast",] 
lifeBoatData <- lifeBoatData[!lifeBoatData$Ticket=="",] 
lifeBoatData <- lifeBoatData[!lifeBoatData$Status=="Unknown",]
lifeBoatData <- lifeBoatData[lifeBoatData$Passenger.Type!="Deck Crew"&
                             lifeBoatData$Passenger.Type!="Engineering Crew" &
                             lifeBoatData$Passenger.Type!="Restaurant Staff" &
                             lifeBoatData$Passenger.Type!="Victualling Crew",]
}
# Imputes the missing Embarked entries for the titanic data.
{
        titanic$Embarked[titanic$Embarked==""] <- "C"
}
# Removes irrelevant variables of the lifeboat data.
{
lifeBoatData <- lifeBoatData[, -c(3,6,9)]
names(lifeBoatData) <- c("Name", "Age", "Class","Embarked","Ticket", "Lifeboat") 
}
# Extracts class of the lifeboat data.
{
lifeBoatData$Class <- as.character(lifeBoatData$Class)
lifeBoatData$Class <- str_sub(lifeBoatData$Class,1,1)
lifeBoatData$Class <- as.integer(lifeBoatData$Class)
}
# Extracts embarkation of the lifeboat data.
{
lifeBoatData$Embarked <- gsub("Cherbourg", "C", lifeBoatData$Embarked, fixed=TRUE)
lifeBoatData$Embarked <- gsub("Cobh", "Q", lifeBoatData$Embarked, fixed=TRUE)
lifeBoatData$Embarked <- gsub("Southampton", "S", lifeBoatData$Embarked, fixed=TRUE)
}
# Modifies the titles in the lifeboat data.
{
lifeBoatData$Name <- gsub("Mrs","Pineapple",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Mr","Mr.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Pineapple","Mrs.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Miss","Miss.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Ms","Miss.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Master","Master.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Captain","Sir.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Colonel","Sir.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Don","Sir.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Father","Sir.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Major","Sir.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Father","Sir.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Reverend","Sir.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Rev","Sir.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Dr","Dr.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Dona","Lady.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Mme","Lady.",lifeBoatData$Name,fixed=TRUE)
lifeBoatData$Name <- gsub("Mlle","Lady.",lifeBoatData$Name,fixed=TRUE)
}
# Extracts surname, title and first name, all in upper case from the lifeboat data.
{
lifeBoatData$Name2 <- toupper(lifeBoatData$Name)
lifeBoatData$Surname<- sapply(lifeBoatData$Name2, 
                              function(x) str_split(x," ")[[1]][1])
lifeBoatData$Title<- sapply(lifeBoatData$Name2, 
                            function(x) str_split(x," ")[[1]][2])
lifeBoatData$Firstname<- sapply(lifeBoatData$Name2, 
                                function(x) str_split(x," ")[[1]][3])
#summary(as.factor(lifeBoatData$Title))
indicesCommonTitlesL <- which(lifeBoatData$Title=="MR."|
                             lifeBoatData$Title=="MRS."|
                             lifeBoatData$Title=="MISS."|
                             lifeBoatData$Title=="MASTER."|
                             lifeBoatData$Title=="DR."|
                             lifeBoatData$Title=="SIR."|
                             lifeBoatData$Title=="LADY.")
#lifeBoatData$Name2[-indicesCommonTitlesL]
indxL <- setdiff(1:nrow(lifeBoatData),indicesCommonTitlesL)
#write.csv(data.frame(indxL,lifeBoatData$Name2[-indicesCommonTitlesL]),"lifeWeirdNames.csv")
tidyNamesL <- read.csv("tidyNamesL.csv")
tidyNamesL$Title <- as.character(tidyNamesL$Title)
tidyNamesL$Surname <- as.character(tidyNamesL$Surname)
tidyNamesL$Firstname <- as.character(tidyNamesL$Firstname)
for(i in 1:nrow(tidyNamesL)){
        index <- tidyNamesL$indxL[i];
        lifeBoatData$Title[index] <- tidyNamesL$Title[i];
        lifeBoatData$Surname[index] <- tidyNamesL$Surname[i];
        lifeBoatData$Firstname[index] <- tidyNamesL$Firstname[i];
}
}
# Extracts surname, title and first name, all in upper case from the titanic data.
{
titanic$Name2 <- toupper(titanic$Name)
titanic$Name2 <- gsub(",","",titanic$Name2,fixed=TRUE)
titanic$Surname<- sapply(titanic$Name2, 
                         function(x) str_split(x," ")[[1]][1])
titanic$Title<- sapply(titanic$Name2, 
                       function(x) str_split(x," ")[[1]][2])
titanic$Firstname<- sapply(titanic$Name2, 
                           function(x) str_split(x," ")[[1]][3])
#summary(as.factor(titanic$Title))
indicesCommonTitlesT <- which(titanic$Title=="MR."|
                              titanic$Title=="MRS."|
                              titanic$Title=="MISS."|
                              titanic$Title=="MASTER."|
                              titanic$Title=="DR."|
                              titanic$Title=="SIR."|
                              titanic$Title=="LADY.")
#titanic$Name2[-indicesCommonTitlesT]
indxT <- setdiff(1:nrow(titanic),indicesCommonTitlesT)
#write.csv(data.frame(indxT,titanic$Name2[-indicesCommonTitlesT]),"titanicWeirdNames.csv")
tidyNamesT <- read.csv("tidyNamesT.csv")
tidyNamesT$Title <- as.character(tidyNamesT$Title)
tidyNamesT$Surname <- as.character(tidyNamesT$Surname)
tidyNamesT$Firstname <- as.character(tidyNamesT$Firstname)
for(i in 1:nrow(tidyNamesT)){
        index <- tidyNamesT$indxT[i];
        titanic$Title[index] <- tidyNamesT$Title[i];
        titanic$Surname[index] <- tidyNamesT$Surname[i];
        titanic$Firstname[index] <- tidyNamesT$Firstname[i];
}
}

# Extracts ticket numbers from the lifeboat data.
{
tickets <- as.character(lifeBoatData$Ticket)
ticketNumber <- sapply(tickets,function(x) strsplit(x,";")[[1]][1])
ticketNumber <- as.vector( ticketNumber)
lifeBoatData$TicketNumber <- ticketNumber       
}

# Creates ShortTicket for each of the lifeboat and titanic data.
{
shortTicket <- function(x){
        result <- x;
        x <- str_split(x," ")
        if(length(x[[1]])>1) result <- x[[1]][2]
        result
}
cleanticketL <- sapply(lifeBoatData$TicketNumber, function(x) shortTicket(x))
lifeBoatData$ShortTicket <- as.vector(cleanticketL)
cleanticketT <- sapply(titanic$Ticket, function(x) shortTicket(x))
titanic$ShortTicket <- as.vector(cleanticketT)
}

# Splits the titanic data into two subsets: with Age and without Age entries.
{
noAge <- which(is.na(titanic$Age))
titanic_NoAge <- titanic[noAge,]
titanic_Age <- titanic[-noAge,]
}



# Merges the lifeboat data to the Titanic_Age data.
# Pass1
{
lifeBoatData$Key <- paste(lifeBoatData$Class,
                          lifeBoatData$Title,
                          lifeBoatData$Firstname,
                          lifeBoatData$Surname,
                          lifeBoatData$Age,
                          sep=" ")
titanic_Age$Key <- paste(titanic_Age$Pclass,
                     titanic_Age$Title,
                     titanic_Age$Firstname,
                     titanic_Age$Surname,
                     titanic_Age$Age,
                     sep=" ")
result_Age1 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age1
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 2
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         lifeBoatData$TicketNumber[-matchedL_Age],
                                         lifeBoatData$Firstname[-matchedL_Age],
                                         lifeBoatData$Age[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        titanic_Age$Ticket[-matchedT_Age],
                                        titanic_Age$Firstname[-matchedT_Age],
                                        titanic_Age$Age[-matchedT_Age],
                                        sep=" ")
result_Age2 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age2
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 3
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         lifeBoatData$TicketNumber[-matchedL_Age],
                                         lifeBoatData$Firstname[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        titanic_Age$Ticket[-matchedT_Age],
                                        titanic_Age$Firstname[-matchedT_Age],
                                        sep=" ")
result_Age3 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age3
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 4
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         lifeBoatData$Surname[-matchedL_Age],
                                         lifeBoatData$Firstname[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        titanic_Age$Surname[-matchedT_Age],
                                        titanic_Age$Firstname[-matchedT_Age],
                                        sep=" ")
result_Age4 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age4
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 5
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         lifeBoatData$Surname[-matchedL_Age],
                                         lifeBoatData$Age[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        titanic_Age$Surname[-matchedT_Age],
                                        titanic_Age$Age[-matchedT_Age],
                                        sep=" ")
result_Age5 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age5
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 6
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         lifeBoatData$ShortTicket[-matchedL_Age],
                                         lifeBoatData$Firstname[-matchedL_Age],
                                         #lifeBoatData$Age[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        titanic_Age$ShortTicket[-matchedT_Age],
                                        titanic_Age$Firstname[-matchedT_Age],
                                        #titanic_Age$Age[-matchedT_Age],
                                        sep=" ")
result_Age6 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age6
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 7
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         lifeBoatData$ShortTicket[-matchedL_Age],
                                         lifeBoatData$Surname[-matchedL_Age],
                                         #lifeBoatData$Age[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        titanic_Age$ShortTicket[-matchedT_Age],
                                        titanic_Age$Surname[-matchedT_Age],
                                        #titanic_Age$Age[-matchedT_Age],
                                        sep=" ")
result_Age7 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age7
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
# This is an unavoidable error to be fixed manuelly.
err <- which(lifeBoatData$Key=="3 MASTER. 2343 SAGE"&lifeBoatData$Age==4)
matchedL_Age <- matchedL_Age[matchedL_Age!=err]

length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 8
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         #lifeBoatData$Firstname[-matchedL_Age],
                                         lifeBoatData$Surname[-matchedL_Age],
                                         #lifeBoatData$Age[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        #titanic_Age$Firstname[-matchedT_Age],
                                        titanic_Age$Surname[-matchedT_Age],
                                        #titanic_Age$Age[-matchedT_Age],
                                        sep=" ")
result_Age8 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age8
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 9
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         #lifeBoatData$Firstname[-matchedL_Age],
                                         lifeBoatData$ShortTicket[-matchedL_Age],
                                         #lifeBoatData$Age[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        #titanic_Age$Firstname[-matchedT_Age],
                                        titanic_Age$ShortTicket[-matchedT_Age],
                                        #titanic_Age$Age[-matchedT_Age],
                                        sep=" ")
result_Age9 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age9
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 10
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Embarked[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         lifeBoatData$Firstname[-matchedL_Age],
                                         #lifeBoatData$ShortTicket[-matchedL_Age],
                                         lifeBoatData$Age[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Embarked[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        titanic_Age$Firstname[-matchedT_Age],
                                        #titanic_Age$ShortTicket[-matchedT_Age],
                                        titanic_Age$Age[-matchedT_Age],
                                        sep=" ")
result_Age10 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age10
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 11
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Embarked[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         lifeBoatData$Firstname[-matchedL_Age],
                                         #lifeBoatData$ShortTicket[-matchedL_Age],
                                         #lifeBoatData$Age[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Embarked[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        titanic_Age$Firstname[-matchedT_Age],
                                        #titanic_Age$ShortTicket[-matchedT_Age],
                                        #titanic_Age$Age[-matchedT_Age],
                                        sep=" ")
result_Age11 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age11
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}
# Pass 12
{
lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                         lifeBoatData$Embarked[-matchedL_Age],
                                         lifeBoatData$Title[-matchedL_Age],
                                         str_sub(lifeBoatData$Surname[-matchedL_Age],1,2),
                                         #lifeBoatData$ShortTicket[-matchedL_Age],
                                         lifeBoatData$Age[-matchedL_Age],
                                         sep=" ")
titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                        titanic_Age$Embarked[-matchedT_Age],
                                        titanic_Age$Title[-matchedT_Age],
                                        str_sub(titanic_Age$Surname[-matchedT_Age],1,2),
                                        #titanic_Age$ShortTicket[-matchedT_Age],
                                        titanic_Age$Age[-matchedT_Age],
                                        sep=" ")
result_Age12 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
result_Age12
matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
length(matchedT_Age); length(matchedL_Age)
length(unique(titanic_Age$Key));length(titanic_Age$Key)
}

# Pass 13
{
        lifeBoatData$Key[-matchedL_Age] <- paste(lifeBoatData$Class[-matchedL_Age],
                                                 lifeBoatData$Embarked[-matchedL_Age],
                                                 lifeBoatData$Title[-matchedL_Age],
                                                 #str_sub(lifeBoatData$Surname[-matchedL_Age],1,2),
                                                 #lifeBoatData$ShortTicket[-matchedL_Age],
                                                 lifeBoatData$Age[-matchedL_Age],
                                                 sep=" ")
        titanic_Age$Key[-matchedT_Age] <- paste(titanic_Age$Pclass[-matchedT_Age],
                                                titanic_Age$Embarked[-matchedT_Age],
                                                titanic_Age$Title[-matchedT_Age],
                                                #str_sub(titanic_Age$Surname[-matchedT_Age],1,2),
                                                #titanic_Age$ShortTicket[-matchedT_Age],
                                                titanic_Age$Age[-matchedT_Age],
                                                sep=" ")
        result_Age13 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
        result_Age13
        matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
        matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
        length(matchedT_Age); length(matchedL_Age)
        length(unique(titanic_Age$Key));length(titanic_Age$Key)
        
        err <- which(lifeBoatData$Key=="3 S MR. 29"&lifeBoatData$Surname!="SCHEERLINCK")
        for(i in 1:length(err))
                lifeBoatData$Key[err[i]] <- paste("3 S MR. 29_",i,sep="")
        err <- which(lifeBoatData$Key=="2 S MR. 32"&lifeBoatData$Surname!="BRITO")
        for(i in 1:length(err))
                lifeBoatData$Key[err[i]] <- paste("2 S MR. 32_",i,sep="")
        
        err <- which(lifeBoatData$Key=="3 S MR. 25")
        for(i in 1:length(err))
                lifeBoatData$Key[err[i]] <- paste("3 S MR. 25_",2*i,sep="")
        #err <- which(titanic_Age$Key=="3 S MR. 25")
        #titanic_Age$Key[err] <- 
        
        result_Age13.5 <- sum(titanic_Age$Key %in% lifeBoatData$Key)/length(titanic_Age$Key)
        result_Age13.5
        matchedT_Age <- which(titanic_Age$Key %in% lifeBoatData$Key)
        matchedL_Age <- which(lifeBoatData$Key %in% titanic_Age$Key)
        length(matchedT_Age); length(matchedL_Age)
        length(unique(titanic_Age$Key));length(titanic_Age$Key)
}


same <- as.factor(titanic_Age$Key)
summary(same)
titanic_Age[which(titanic_Age$Key=="3 MR. 2. MATTI"), ]
