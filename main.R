# Define libraries.
library(knitr)
library(reshape2)
library(ggplot2)
library(doParallel)
library(pROC)

# Register the number of cores.
registerDoParallel(cores=detectCores())

# Define libraries.
library(caret)
library(ggplot2)

# Load helper functions.
source("readCleanData.R")
source("filterFeaturesByName.R")

# Load training and evaluation data.
cleanData <- readCleanData("pml-training.csv", na.thres = 0.1)
dim(cleanData)
cleanEval <- readCleanData("pml-testing.csv" , na.thres = 0.1)
dim(cleanEval)

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
data <- filterFeaturesByName(cleanData, nonReqFeatures)
dim(data)
eval <- filterFeaturesByName(cleanEval, nonReqFeatures)
dim(eval)

# Partition cleaned and filtered data into the training and test sets.
set.seed(1983)
parts <- createDataPartition(data$classe, p=0.7, list=FALSE)
train <- data[parts, ]
test <- data[-parts, ]

# Keep track of the start time.
start = Sys.time()

# Fit first model using the 10-fold cross-validation resampling.
modFitRF <- train(classe ~ ., data = train, method = "rf", trControl = trainControl(method = "cv",number = 5), prox = TRUE, importance = TRUE)
modFitRF
modFitRF$finalModel

# Predict test classes using random forest model.
predRF <- predict(modFitRF, test[, -49])
cmRF <- confusionMatrix(predRF, test$classe)
cmRF

# Calculate the class range to select from the finalModel confustion matrix.
classRange <- seq_along(modFitRF$finalModel$classes)

# Calculate the estimated out-of-sample (OOB) error from the finalModel confustion matrix as a percentage.
estimatedErr <- round((1 - (sum(diag(modFitRF$finalModel$confusion)) / sum(modFitRF$finalModel$confusion[classRange, classRange]))) * 100, 2)

# Calculate the actual out-of-sample error from the confusion matrix taken against the test set as a percentage.
actualErr <- round((1 - cmRF$overall[1]) * 100, 2)

# Finally calculate the total running time in seconds.
totTime <- as.integer(Sys.time() - start)

