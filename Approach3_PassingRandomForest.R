set.seed(1)
library(ISLR)
library(tree)
library(randomForest)
#Data management
#Import dataset for NFL_Passing.csv
passingData = read.csv('NFL_Passing.csv')
#Use the first 400 samples for training and the other 112 for testing
trainingData = passingData[1:400,]
testingData = passingData[401:512,]
#Extract values for labels, QBR, PCT, etc.
#Extraction is not used for training, but is useful for future use
labelsTrain = unlist(trainingData[14])
QBRTrain = unlist(trainingData[12])
PCTTrain = unlist(trainingData[6])
YdsGTrain = unlist(trainingData[9])
TDTrain = unlist(trainingData[10])
#Train the logistic model for passing
passRandomForestModel <- randomForest(Label ~ TD + QBR + Pct + Yds + YdsG + Int, data=trainingData)
summary(passRandomForestModel)

#Extract values for Testing Data
labelsTest = unlist(testingData[14])
QBRTest = unlist(testingData[12])
IntTest = unlist(testingData[11])
PCTTest = unlist(testingData[6])
YdsGTest = unlist(testingData[9])
TDTest = unlist(testingData[10])
YdsTest = unlist(testingData[7])
#Store the data into a dataframe
mainData <- data.frame(TD=TDTest, QBR = QBRTest, Pct = PCTTest, Yds =YdsTest, YdsG = YdsGTest, Int = IntTest);
#Obtain predictions
predictionsMain <- predict(passRandomForestModel, mainData,type="response")

#Evaluate prediction accuracy
#Use threshold of 0.5 to determine class boundary
firstParam = 1
secondParam = 0
finalPreds = as.data.frame(predictionsMain)
#Cycles through the the predictions (predictionsMain) and uses the first and second parameters for filtering.
#adjusts finalPreds to either 1 or 0 based on classification
for (x in predictionsMain)
{
  #Set threshold at 0.5
  if (x > 0.50)
  {
    #Cycles through the the predictions and uses the first and second parameters for filtering.
    #adjusts finalPreds to either 1 or 0 based on classification
    predictionsMain[firstParam] <- 1
    secondParam = secondParam + 1;
  }
  else
  {
    predictionsMain[firstParam] <- 0
  }
  firstParam = firstParam + 1
}
finalPreds = as.data.frame(predictionsMain)
targets = as.data.frame(labelsTest)

#Analyze how many of the test samples were accurate
accurate = 0
for (x in 1:112)
{
  if(finalPreds[x,]==targets[x,])
  {
    accurate = accurate + 1;
  }
}
accuracyRate = accurate/112



