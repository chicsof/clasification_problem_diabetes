#Predicting whether or not a patient is diabetic
data <- read.csv(file = "Pima.te.csv", head = TRUE, sep = ",")
head(data)

install.packages("caTools")
library(caTools)
split <- sample.split(data, SplitRatio =  0.8)
split

trainingData <- subset(data, split ==TRUE)
testingData <- subset(data, split==FALSE)

trainingData

#type = 1 patient is diabetic
model <- glm(type ~.-skin, trainingData,family = "binomial" )
summary(model)

#response gives propability
#this is just to give us an idea, stores the propabilities under res
res <- predict(model, testingData, type = "response")
res

#confusion matrix
table(ActualValue=testingData$type, PredictedValue = res>0.55)

#acuraccy/values are taken form confusion matrix
(43+16)/(7+8+43+16)

#Choosing a better threshold
install.packages("ROCR")
library(ROCR)

resForTraining <-predict(model, trainingData, type = "response")
#choosing a value
ROCRPred = prediction(resForTraining,trainingData$type)
#measuring performance true and false predicted rate (0,1)
ROCRPref <-performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref, colorize = TRUE)

##################################################################################
#############SOLVING THE PROBLEM USING DESITION TREES#############################
##################################################################################

library(rpart)
model<- rpart (type ~., data = trainingData)
plot(model, margin = 0.1)
text(model, use.n = TRUE, pretty=TRUE)
library(caret)
predicted <- predict(model, newdata = testingData, type="class")
predicted
confusionMatrix(table(predicted, testingData$type))



##################################################################################
#############SOLVING THE PROBLEM USING RANDOM FOREST #############################
##################################################################################
library(randomForest)

trainingData$type <- as.factor(trainingData$type)
testingData$type <- as.factor(testingData$type)

#for having variety in decirtion trees. Each tree will take one combination of mtry (3 in this case) attributes
#this is choosen by constantly iterating until viriety is improved
bestmrtry <- tuneRF(trainingData, trainingData$type, stepFactor= 1.2, improve =0.01, trace = T, plot = T)

model <-randomForest(type  ~., data = trainingData)
model

model$importance

#turns out this is most accurate
library(caret)
predicted <- predict(model, newdata = testingData, type="class")
predicted
confusionMatrix(table(predicted, testingData$type))

varImpPlot(model)
