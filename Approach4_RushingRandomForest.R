set.seed(1)
library(ISLR)
library(tree)
library(randomForest)
#Data management
#Import dataset for NFL_Rushing.csv
rushingData = read.csv('NFL_Rushing.csv')
#Use the first 400 samples for training and the other 112 for testing
trainingData = rushingData[1:400,]
testingData = rushingData[401:512,]
#Extract values for labels, QBR, PCT, etc.
labelsTrain = unlist(trainingData[14])
YdsTrain = unlist(trainingData[5])
YdsGTrain = unlist(trainingData[7])
TDTrain = unlist(trainingData[8])
FumLTrain = unlist(trainingData[11])
AvgYdsTrain = unlist(trainingData[6])
FumTrain = unlist(trainingData[10])
#Train the logistic model for passing
rushRandomForestModel <- randomForest(Label ~ TD + Yds + Yds.G + Fum + FumL + Avg, data=trainingData)
summary(rushRandomForestModel)

#Extract values for Testing Data
labelsTest = unlist(testingData[14])
YdsTest = unlist(testingData[5])
YdsGTest = unlist(testingData[7])
TDTest = unlist(testingData[8])
FumLTest = unlist(testingData[11])
AvgYdsTest = unlist(testingData[6])
FumTest = unlist(testingData[10])
#Store the data into a dataframe
mainData <- data.frame(TD=TDTest, Yds=YdsTest, Yds.G = YdsGTest, Fum=FumTest, FumL=FumLTest,Avg=AvgYdsTest);
#Obtain predictions
predictionsMain <- predict(rushRandomForestModel, mainData,type="response")

#Evaluate prediction accuracy
#Use threshold of 0.5 to determine class boundary
firstParam = 1
secondParam = 0
finalPreds = as.data.frame(predictionsMain)
for (x in predictionsMain)
{
  if (x > 0.50)
  {
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
#To obtain accuracy rate divide the total accurate values by the total testing count (112)
accuracyRate = accurate/112



