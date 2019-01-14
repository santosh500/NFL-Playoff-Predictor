library(Hmisc)
set.seed(1)
library(ISLR)
library(tree)
library(randomForest)
#Data management
#Import dataset for NFL_Rushing.csv
rushingData = read.csv('NFL_Rushing.csv')
#Use the first 400 samples for training and the other 112 for testing
trainingRushData = rushingData[1:400,]
testingRushData = rushingData[401:512,]
#Extract values for labels, QBR, PCT, etc.
labelsTrain = unlist(trainingRushData[14])
YdsTrain = unlist(trainingRushData[5])
YdsGTrain = unlist(trainingRushData[7])
TDTrain = unlist(trainingRushData[8])
FumLTrain = unlist(trainingRushData[11])
AvgYdsTrain = unlist(trainingRushData[6])
FumTrain = unlist(trainingRushData[10])


#Data management
#Import dataset for NFL_Passing.csv
passingData = read.csv('NFL_Passing.csv')
#Use the first 400 samples for training and the other 112 for testing
totalData <- merge(passingData,rushingData, by=c("YEAR","Team","GP"))
trainingPassData = passingData[1:400,]
testingPassData = passingData[401:512,]
labelsTrain = unlist(trainingPassData[14])
QBRTrain = unlist(trainingPassData[12])
PCTTrain = unlist(trainingPassData[6])
YdsGTrain = unlist(trainingPassData[9])
TDTrain = unlist(trainingPassData[10])
#Train the random forest for combined
combinedRandomForestModel <- randomForest(Label.x ~ TD.y + Yds.y + Yds.G + Fum + FumL + Avg.y + TD.x + QBR + Pct + Yds.x + YdsG + Int, data=totalData)
summary(combinedRandomForestModel)

#Extract values for Testing Data for both Passing and Rushing
QBRTestPass = unlist(testingPassData[12])
IntTestPass = unlist(testingPassData[11])
PCTTestPass = unlist(testingPassData[6])
YdsGTestPass = unlist(testingPassData[9])
TDTestPass = unlist(testingPassData[10])
YdsTestPass = unlist(testingPassData[7])


labelsTest = unlist(testingRushData[14])
YdsTest = unlist(testingRushData[5])
YdsGTest = unlist(testingRushData[7])
TDTest = unlist(testingRushData[8])
FumLTest = unlist(testingRushData[11])
AvgYdsTest = unlist(testingRushData[6])
FumTest = unlist(testingRushData[10])

#Store the data into a dataframe
mainData <- data.frame(TD.y=TDTest, Yds.y=YdsTest, Yds.G = YdsGTest, Fum=FumTest, FumL=FumLTest, Avg.y=AvgYdsTest, TD.x=TDTestPass,QBR=QBRTestPass,Pct = PCTTestPass,Yds.x = YdsTestPass,YdsG=YdsGTestPass,Int=IntTestPass);

#Obtain predictions
predictionsMain <- predict(combinedRandomForestModel, mainData,type="response")
predGeneral <- as.data.frame(predictionsMain)
#Evaluate prediction accuracy
#Use threshold of 0.5 to determine class boundary
firstParam = 1
secondParam = 0
finalPreds = as.data.frame(predictionsMain)
for (x in predictionsMain)
{
  if (x > 0.5)
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

