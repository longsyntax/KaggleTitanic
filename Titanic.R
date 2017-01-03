setwd("C:/Users/Abhay Bhat/Documents/Personal/Titanic")

library(caret)

trainingSet <- read.csv("train.csv")
testSet <- read.csv("test.csv")

# --- Exploring features ---
colnames(trainingSet)
summary(trainingSet$Survived)
table(trainingSet$Survived)
summary(trainingSet$Pclass)
table(trainingSet$Pclass)
trainingSet$Survived <- as.factor(trainingSet$Survived)
trainingSet$Pclass <- as.factor(trainingSet$Pclass)
plot(trainingSet$Pclass, trainingSet$Survived)

# --- Exploring featues ---
summary(trainingSet$Sex)
trainingSet$Sex <- as.factor(trainingSet$Sex)
plot(trainingSet$Sex, trainingSet$Survived)
summary(trainingSet$Age)
summary(trainingSet$Fare)

# --- Improving data quality --- 
# - Some fares are 0 - looking further into it - replacing 0s with med value of Pclass
subset(trainingSet, Fare < 1, select = c(Age, Pclass, Fare, Sex))
aggregate(trainingSet$Fare, by = list(trainingSet$Pclass), FUN = mean, na.rm = TRUE)
aggregate(trainingSet$Fare, by = list(trainingSet$Pclass), FUN = median, na.rm = TRUE)
trainingSet$Fare <- ifelse((round(trainingSet$Fare==0) & as.numeric(trainingSet$Pclass)==1),60.2875,
                    ifelse((round(trainingSet$Fare==0) & as.numeric(trainingSet$Pclass)==2),14.25,
                    ifelse((round(trainingSet$Fare==0) & as.numeric(trainingSet$Pclass)==3),8.05,
                    trainingSet$Fare)))
summary(trainingSet$Fare)
subset(trainingSet, Fare < 5, select = c(Age, Pclass, Fare, Sex))

# --- Exploring features --- 
summary(trainingSet$SibSp)
table(trainingSet$SibSp, trainingSet$Survived)
summary(trainingSet$Name)
is.na(trainingSet$Name)

# - Extracting just titles from $Name
trainingSet$Title <- ifelse(grepl("mr", tolower(trainingSet$Name)), 'Mr', 
                     ifelse(grepl("miss", tolower(trainingSet$Name)), 'Miss', 
                     ifelse(grepl("mrs", tolower(trainingSet$Name)), 'Mrs', 
                     ifelse(grepl("master", tolower(trainingSet$Name)), 'Master',
                     'Unknown')))) 
summary(trainingSet$Title)
trainingSet$Title


# --- Building Age model to predict values for NA's ---
summary(trainingSet$Age)
age.model<-lm(Age ~ Title + Fare + SibSp + Sex -1, data = trainingSet)
summary(age.model)

# - Fitting values for age LM
for(i in 1:nrow(trainingSet))
{
  if(is.na(trainingSet[i,"Age"]))
  {
    trainingSet[i,"Age"] <- predict(age.model, newdata=trainingSet[i,])
  }
  
}
summary(trainingSet$age)

# - Replacing one negative age 
subset(trainingSet, Age < 0, select = Name, Age)
trainingSet$Age <- ifelse(trainingSet$Age<0, (-1*trainingSet$Age), trainingSet$Age)

# - New variable for Youngsters (<= 21 y.o)
trainingSet$Young <- ifelse(trainingSet$Age <= 21, 1, 0)
table(trainingSet$Young, trainingSet$Survived)
table(trainingSet$Young, trainingSet$Survived, by=trainingSet$Sex)

# - New variable for Young or Female
trainingSet$Female <- ifelse(trainingSet$Sex=="female",1,0)
trainingSet$YoungorFemale <- ifelse(trainingSet$Young==1 | trainingSet$Female==1,1,0)
table(trainingSet$YoungorFemale, trainingSet$Survived)

# --- Exploring features ---
summary(trainingSet$Parch)
summary(trainingSet$SibSp)

# - New variables for family size range
trainingSet$FamilySize <- trainingSet$SibSp + trainingSet$Parch + 1
trainingSet$FamilySize <- cut(trainingSet$FamilySize, c(0,1,4,20))
table(trainingSet$FamilySize, trainingSet$Survived)

# --- Creating the model ---
model <- train(Survived ~ Pclass + Sex + YoungorFemale + FamilySize + Title, 
               data = trainingSet, 
               method = "rf", 
               trControl = trainControl(method = "cv", number = 5))
model


# --- Recreating same features in the testing set ---
testSet$thirdClass <- ifelse(testSet$Pclass=="3",1,0)
testSet$Fare <- ifelse((round(testSet$Fare==0) & as.numeric(testSet$Pclass)==1),60.2875,
                ifelse((round(testSet$Fare==0) & as.numeric(testSet$Pclass)==2),14.25,
                ifelse((round(testSet$Fare==0) & as.numeric(testSet$Pclass)==3),8.05,
                testSet$Fare)))
testSet$Title <- ifelse(grepl("mr", tolower(testSet$Name)), 'Mr', 
                 ifelse(grepl("miss", tolower(testSet$Name)), 'Miss', 
                 ifelse(grepl("mrs", tolower(testSet$Name)), 'Mrs', 
                 ifelse(grepl("master", tolower(testSet$Name)), 'Master',
                 'Unknown'))))
age.model<-lm(Age ~ Title + Fare + SibSp + Sex -1, data = testSet)
for(i in 1:nrow(testSet))
{
  if(is.na(testSet[i,"Age"]))
  {
    testSet[i,"Age"] <- predict(age.model, newdata=testSet[i,])
  }
  
}
testSet$Age <- ifelse(testSet$Age<0, (-1*testSet$Age), testSet$Age)
testSet$Young <- ifelse(testSet$Age <= 21, 1, 0)
testSet$Female <- ifelse(testSet$Sex=="female",1,0)
testSet$YoungorFemale <- ifelse(testSet$Young==1 | testSet$Female==1,1,0)
testSet$FamilySize <- testSet$SibSp + testSet$Parch + 1
testSet$FamilySize <- cut(testSet$FamilySize, c(0,1,4,20))

# --- Prediction on the test set and submission creation --- 
testSet$Survived <- predict(model, newdata = testSet)
summary(testSet)
submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "submission2.csv", col.names = TRUE, row.names = FALSE, sep = ",")











