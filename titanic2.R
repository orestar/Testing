library(caret); library(randomForest)

# Loads and preprocesses the data
titanic <- read.csv("train.csv")

# Exploratory summary
summary(titanic)


# Imputes the missing embarkation entries
titanic$Embarked[titanic$Embarked==""] <- "S"


### Since about 20% of the age entries were missing, we first impute the ages
### by the implication from the titles.
# Extracts titles and lastnames
names <- as.character(titanic$Name)
lastname <- sapply(names,function(x) strsplit(x,",")[[1]][1])
titanic$Lastname <- as.vector(lastnames)
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
imputeDrAge <- median(titanic$Age[titanic$Title=="Dr."&titanic$Sex=="male"],na.rm=TRUE)
titanic$Age[titanic$Title=="Mr." & is.na(titanic$Age)] <- imputeMrAge
titanic$Age[titanic$Title=="Mrs." & is.na(titanic$Age)] <- imputeMrsAge
titanic$Age[titanic$Title=="Miss." & is.na(titanic$Age)] <- imputeMissAge
titanic$Age[titanic$Title=="Master." & is.na(titanic$Age)] <- imputeMasterAge
titanic$Age[titanic$Title=="Dr." & is.na(titanic$Age)] <- imputeDrAge

# Derives family relations
titanic$FamSize <- as.vector(titanic$SibSp+titanic$Parch+c(1))
titanic2 <- titanic[,c(12,3,11,13,14,6,15,5,10,2)]
titanic2 <- titanic2[order(titanic2$Embarked,titanic2$Pclass,titanic2$Cabin,
                           titanic2$Lastname,titanic2$Fare,titanic2$FamSize),]
titanic2$Title[titanic2$Title=="Don."] <- "Sir."
titanic2$Title[titanic2$Title=="Rev."] <- "Mr."
titanic2$Title[titanic2$Title=="Mme."] <- "Lady."
titanic2$Title[titanic2$Title=="Major."] <- "Sir."
titanic2$Title[titanic2$Title=="Mlle."] <- "Miss."
titanic2$Title[titanic2$Title=="Col."] <- "Sir."
titanic2$Title[titanic2$Title=="Capt."] <- "Sir."
titanic2$Title[titanic2$Sex=="male"  & 
                       !titanic2$Title %in% c("Mr.","Master.","Sir.")] <- "Sir."
titanic2$Title[titanic2$Sex=="female"  & 
                       !titanic2$Title %in% c("Mrs.","Miss.","Ms.","Lady.")] <- "Lady."
