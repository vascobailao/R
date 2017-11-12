setwd("/Users/vascofernandes/Desktop/projSAD")
library(caret)

crabs_dataset <- read.csv("crabs.csv")
crabs_dataset$index <- NULL
apply(crabs_dataset, 2, function(x) any(is.na(x) | is.infinite(x)))
head(crabs_dataset)
summary(crabs_dataset)
str(crabs_dataset)
table(crabs_dataset$sp)
table(crabs_dataset$sex)

# crabs

# kNN
ind <- sample(2, nrow(crabs_dataset), replace=TRUE, prob=c(0.7, 0.3))
trainData <- crabs_dataset[ind==1,]
testData <- crabs_dataset[ind==2,]

trainData$sex <- NULL
testData$sex <- NULL
model_knn = train(trainData[,2:6], trainData$sp, method = 'knn')
predictions<-predict(object=model_knn,testData[,2:6])
table(predictions)
consufion_matrix <- confusionMatrix(predictions,testData$sp)
train_control <- trainControl(method="cv", number=10)

# NB
library(e1071)
crabs_dataset$sex <- NULL
x = crabs_dataset[2:6]
y = crabs_dataset$sp
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
predict(model$finalModel,x)
table(predict(model$finalModel,x)$class,y)
naive_crabs <- NaiveBayes(crabs_dataset$sp ~ ., data = crabs_dataset)
plot(naive_crabs)


# Decision Tree (CART, C4.5, SMOTE)´

# CART
library(rpart)
crabs_dataset$sex <- NULL
fit <- rpart(sp~., data=crabs_dataset)
summary(fit)
predictions <- predict(fit, crabs_dataset[,2:6], type="class")
table(predictions, crabs_dataset$sp)

#C4.5

# load the package
library(RWeka)
# fit model
fit <- J48(sp~., data=crabs_dataset)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, crabs_dataset[,2:6])
# summarize accuracy
table(predictions, crabs_dataset$sp)

# NN
library(RSNNS)
crabs_dataset <- read.csv("crabs.csv")
crabs_dataset$sex <- NULL

#shuffle the vector
crabs_dataset <- crabs_dataset[sample(1:nrow(crabs_dataset),length(1:nrow(crabs_dataset))),1:ncol(crabs_dataset)]

crabsValues <- crabs_dataset[,2:6]
crabsTargets <- decodeClassLabels(crabs_dataset[,1])

crabs_dataset<- splitForTrainingAndTest(crabsValues, crabsTargets, ratio=0.15)
crabs_dataset <- normTrainingAndTestSet(crabs_dataset)

model <- mlp(crabs_dataset$inputsTrain, crabs_dataset$targetsTrain, size=5, learnFuncParams=c(0.1), 
             maxit=50, inputsTest=crabs_dataset$inputsTest, targetsTest=crabs_dataset$targetsTest)

summary(model)
model
weightMatrix(model)
extractNetInfo(model)

par(mfrow=c(2,2))
plotIterativeError(model)

predictions <- predict(model,crabs_dataset$inputsTest)

plotRegressionError(predictions[,2], crabs_dataset$targetsTest[,2])

confusionMatrix(crabs_dataset$targetsTrain,fitted.values(model))
confusionMatrix(crabs_dataset$targetsTest,predictions)

plotROC(fitted.values(model)[,2], crabs_dataset$targetsTrain[,2])
plotROC(predictions[,2], crabs_dataset$targetsTest[,2])

#confusion matrix with 402040-method
confusionMatrix(crabs_dataset$targetsTrain, encodeClassLabels(fitted.values(model), method="402040", l=0.4, h=0.6))

## noshows dataset
noshow_dataset <- read.csv("noshows.20171013.csv")
library(readr)
noshows_20171013 <- read_delim("noshows.20171013.csv",";", escape_double = FALSE, trim_ws = TRUE)
install.packages('e1071', dependencies=TRUE)

noshows_20171013$PatientId <- NULL
noshows_20171013$AppointmentID <- NULL
noshows_20171013$ScheduledDay <- NULL
noshows_20171013$AppointmentDay <- NULL
noshows_20171013$Neighbourhood <- NULL
noshows_20171013$Gender = as.double(noshows_20171013$Gender=="M")
noshows_20171013[1:13] <- lapply(noshows_20171013[1:13], as.numeric)
noshows_20171013 <- as.data.frame(noshows_20171013)
apply(noshows_20171013, 2, function(x) any(is.na(x) | is.infinite(x)))

