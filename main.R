# Define libraries.
library(caret)
library(ggplot2)
library(randomForest)
#library(doMC)

#registerDoMC(cores = 4)

# Load helper functions.
source("readCleanData.R")
source("filterFeaturesByName.R")

# Load training and evaluation data.
cleanData <- readCleanData("pml-training.csv")
cleanEval <- readCleanData("pml-testing.csv")

# Define the list of non-required features.
nonReqFeatures <- c("X", 
                    "user_name", 
                    "raw_timestamp_part_1", 
                    "raw_timestamp_part_2", 
                    "cvtd_timestamp", 
                    "new_window", 
                    "num_window", 
                    "total_accel_belt", 
                    "total_accel_arm", 
                    "total_accel_dumbbell", 
                    "total_accel_forearm")

# Remove non-required features.
#data <- filterFeaturesByName(cleanData, nonReqFeatures)
#eval <- filterFeaturesByName(cleanEval, nonReqFeatures)

reqFeatures <- c("roll_belt", "pitch_belt", "yaw_belt", "roll_arm", "pitch_arm", "yaw_arm", "roll_forearm", "pitch_forearm", "yaw_forearm", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell")
data <- cleanData[, c(reqFeatures, "classe")]
eval <- cleanEval[, c(reqFeatures, "problem_id")]

# Correlated data can be found using the following below. But it was
# decided that this does affect the accuracy in any way. This the
# only correlated feature `accel_belt_z` was retained.
#dataCor <- cor(data[, -49]); 
#highDataCor <- findCorrelation(dataCor, cutoff=.99)

# Partition cleaned and filtered data into the training and test sets.
set.seed(1983)
parts <- createDataPartition(data$classe, p=0.7, list=FALSE)
train <- data[parts, ]
test <- data[-parts, ]

#print(sprintf("Start modRfPlain = %s", Sys.time()))
#rf_model_plain <- randomForest(classe ~., data=train, importance=TRUE)
#print(sprintf("End modRfPlain = %s", Sys.time()))


print(sprintf("Start modFitx = %s", Sys.time()))
rf_model_rpy_2<-train(x=train[, -13], y=train[, 13],method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)
print(sprintf("End modFitx = %s", Sys.time()))
print(rf_model_rpy)

# Fit first model using the 5-fold cross-validation resampling.
#print(sprintf("Start modFit = %s", Sys.time()))
#modFitRFCV <- train(classe ~ ., data = train, method = "rf", trControl = trainControl(method = "cv", number = 5), prox = TRUE, importance = TRUE)
#print(sprintf("End modFit = %s", Sys.time()))

# Fit a second model using caret default settings of bootstrap resampling.
#print(sprintf("Start modFit2 = %s", Sys.time()))
#modFitRFBS <- train(classe ~ ., data = train, method ="rf", prox = TRUE, importance = TRUE)
#print(sprintf("End modFit2 = %s", Sys.time()))

# Fit a third model, this time using CART.
#print(sprintf("Start modFit3 = %s", Sys.time()))
#modFitCART <- train(classe ~ ., data = train, method="rpart")
#print(sprintf("End modFit3 = %s", Sys.time()))

# Use models to perform predictions on the test set.
#predictions <- predict(modFit, test)
#predictions2 <- predict(modFit2, test)
#predictions3 <- predict(modFit3, test)

# Create confusion matrices for each.
#confusionMatrix(predictions, test$classe)
#confusionMatrix(predictions2, test$classe)
#confusionMatrix(predictions3, test$classe)