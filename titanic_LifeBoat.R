####################  Loading and Processing  ################################
library(caret); library(randomForest); library(reshape2); library(stringr)

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

# Extracts the titles and surnames from the Titanic data for later use.
{
        names <- as.character(titanic$Name)
        lastname <- sapply(names,function(x) strsplit(x,",")[[1]][1])
        titanic$Lastname <- as.vector(lastname)
        title <- sapply(names,function(x) strsplit(x,",")[[1]][2])
        title <- as.vector(title)
        title <- sapply(title,function(x) strsplit(x," ")[[1]][2])
        title <- as.vector(title)
        titanic$Title <- title
}


# Merges the lifeboat data to the Titanic data.
{
        lifeBoatData <- lifeBoatData[, -c(5,6,9)]
        names(lifeBoatData) <- c("Name", "Age", "Status", "Class", "Ticket", "Lifeboat") 
        lifeBoatData <- lifeBoatData[!lifeBoatData$Ticket=="",] 
        lifeBoatData <- lifeBoatData[!lifeBoatData$Status=="Unknown",]
        lifeBoatData <- lifeBoatData[
                lifeBoatData$Class!="Deck Crew"&
                        lifeBoatData$Class!="Engineering Crew" &
                        lifeBoatData$Class!="Restaurant Staff" &
                        lifeBoatData$Class!="Victualling Crew", 
                ]
        lifeBoatData$Class <- as.character(lifeBoatData$Class)
        lifeBoatData$Class <- str_sub(lifeBoatData$Class,1,1)
        lifeBoatData$Class <- as.integer(lifeBoatData$Class)
        
        
        lifeBoatData$Name <- gsub("Mrs","Pineapple",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Mr","Mr.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Pineapple","Mrs.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Miss","Miss.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Master","Master.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Captain","Capt.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Colonel","Col.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Don","Don.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Father","Rev.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Major","Major.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Don","Don.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Father","Rev.",lifeBoatData$Name,fixed=TRUE)
        lifeBoatData$Name <- gsub("Reverend","Rev.",lifeBoatData$Name,fixed=TRUE)
        
        
        lifeBoatData$Name2 <- toupper(lifeBoatData$Name)
        lifeBoatData$Surname<- sapply(lifeBoatData$Name2, 
                                      function(x) str_split(x," ")[[1]][1])
        lifeBoatData$Title<- sapply(lifeBoatData$Name2, 
                                      function(x) str_split(x," ")[[1]][2])
        lifeBoatData$Firstname<- sapply(lifeBoatData$Name2, 
                                    function(x) str_split(x," ")[[1]][3])
        
        # First pass
        lifeBoatData$Key <- paste(lifeBoatData$Class,
                                  lifeBoatData$Surname,
                                  lifeBoatData$Firstname,
                                  sep=" ")
        titanic$Name2 <- toupper(titanic$Name)
        titanic$Name2 <- gsub(",","",titanic$Name2,fixed=TRUE)
        titanic$Surname<- sapply(titanic$Name2, 
                                      function(x) str_split(x," ")[[1]][1])
        titanic$Title<- sapply(titanic$Name2, 
                                    function(x) str_split(x," ")[[1]][2])
        titanic$Firstname<- sapply(titanic$Name2, 
                                        function(x) str_split(x," ")[[1]][3])
        titanic$Key <- paste(titanic$Pclass,
                             titanic$Surname,
                             titanic$Firstname,
                             sep=" ")
        result1 <- sum(titanic$Key %in% lifeBoatData$Key)/length(titanic$Key)
        matchedT <- which(titanic$Key %in% lifeBoatData$Key)
        matchedL <- which(lifeBoatData$Key %in% titanic$Key)
        
        # Second pass
        tickets <- as.character(lifeBoatData$Ticket)
        ticketNumber <- sapply(tickets,function(x) strsplit(x,";")[[1]][1])
        ticketNumber <- as.vector( ticketNumber)
        lifeBoatData$TicketNumber <- ticketNumber
        lifeBoatData$Key[-matchedL] <- paste(lifeBoatData$Class[-matchedL],
                                             lifeBoatData$TicketNumber[-matchedL],
                                             lifeBoatData$Firstname[-matchedL],
                                             sep=" ")
        titanic$Key[-matchedT] <- paste(titanic$Pclass[-matchedT],
                             titanic$Ticket[-matchedT],
                             titanic$Firstname[-matchedT],
                             sep=" ")
        result2 <- sum(titanic$Key %in% lifeBoatData$Key)/length(titanic$Key)
        matchedT <- which(titanic$Key %in% lifeBoatData$Key)
        matchedL <- which(lifeBoatData$Key %in% titanic$Key)
                
        # Third pass
        cleanTicket <- function(x){
                result <- x;
                x <- str_split(x," ")
                if(length(x[[1]])>1) result <- x[[1]][2]
                result
        }
        cleanticketL <- sapply(lifeBoatData$TicketNumber[-matchedL], function(x) cleanTicket(x))
        cleanticketL <- as.vector(cleanticketL)
        lifeBoatData$Key[-matchedL] <- paste(lifeBoatData$Class[-matchedL],
                                             cleanticketL,
                                             lifeBoatData$Firstname[-matchedL],
                                             sep=" ")
        cleanticketT <- sapply(titanic$Ticket[-matchedT], function(x) cleanTicket(x))
        cleanticketT <- as.vector(cleanticketT)
        titanic$Key[-matchedT] <- paste(titanic$Pclass[-matchedT],
                                        cleanticketT,
                                        titanic$Firstname[-matchedT],
                                        sep=" ")
        result3 <- sum(titanic$Key %in% lifeBoatData$Key)/length(titanic$Key)
        matchedT <- which(titanic$Key %in% lifeBoatData$Key)
        matchedL <- which(lifeBoatData$Key %in% titanic$Key)
        
        
        # Fourth pass
        cleanticketL <- sapply(lifeBoatData$TicketNumber[-matchedL], function(x) cleanTicket(x))
        cleanticketL <- as.vector(cleanticketL)
        lifeBoatData$Key[-matchedL] <- paste(lifeBoatData$Class[-matchedL],
                                             cleanticketL,
                                             lifeBoatData$Age[-matchedL],
                                             sep=" ")
        cleanticketT <- sapply(titanic$Ticket[-matchedT], function(x) cleanTicket(x))
        cleanticketT <- as.vector(cleanticketT)
        titanic$Key[-matchedT] <- paste(titanic$Pclass[-matchedT],
                                        cleanticketT,
                                        titanic$Age[-matchedT],
                                        sep=" ")
        result4 <- sum(titanic$Key %in% lifeBoatData$Key)/length(titanic$Key)
        matchedT <- which(titanic$Key %in% lifeBoatData$Key)
        matchedL <- which(lifeBoatData$Key %in% titanic$Key)
        
        
}

titanic$Name2 <- toupper(titanic$Name)
titanic <- titanic[order(titanic$Pclass,titanic$Name2),]
write.csv(titanic,"titanic.csv")
lifeBoatData$Name2 <- toupper(lifeBoatData$Name)
lifeBoatData <- lifeBoatData[order(lifeBoatData$Class,lifeBoatData$Name2),]
write.csv(lifeBoatData,"lifeBoat.csv")










# Simplifies the titles.
{
        titanic$Title[titanic$Title=="Don."] <- "Sir."
        titanic$Title[titanic$Title=="Rev."] <- "Sir."
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

