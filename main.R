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

# Plot a panel of histograms, one for each feature.
meltHist <- melt(data[, -49], id.vars = NULL, value.name = "Value", variable.name = "Feature")
ggplot(data = meltHist, aes(x = Value)) + 
  facet_wrap(~Feature, scales = "free_x", ncol = 4) + 
  geom_histogram() +
  xlab("Feature Value") + ylab("Observation Frequency") + ggtitle("Selected Features vs. Frequency")

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

# Calculate the estimated out-of-sample (or OOB) error from the finalModel confustion matrix as a percentage.
estimatedErr <- round((1 - (sum(diag(modFitRF$finalModel$confusion)) / sum(modFitRF$finalModel$confusion[classRange, classRange]))) * 100, 2)

# Calculate the actual out-of-sample error from the confusion matrix taken against the test set as a percentage.
actualErr <- round((1 - cmRF$overall[1]) * 100, 2)

# Finally calculate the total running time in seconds.
totTime <- as.integer(Sys.time() - start)

# Calculate the difference between each error position in OOB for each generated tree.
# This we will use to try to get an estimate of where an 'elbow' in the error
# plot is found. This will allow us to identify the point in the error plot where
# the error rate was decreasing very slowly, compared to the number of trees.
smallDeltaThres = 0.0005
firstErr <- c(modFitRF$finalModel$err.rate[, 1], 0)
secondErr <- c(0, modFitRF$finalModel$err.rate[, 1])

# Find the absolute error between the OOB of each tree.
absErrDiff <- abs(firstErr - secondErr)[-length(firstErr)]

# Find the index of the tree at which the error changes were very small. This 
# value will be used to plot a line on the error figure.
elbow <- (length(firstErr) - 1) - match(FALSE, rev(absErrDiff < smallDeltaThres))

# Plot the number of trees forest against the error for each class.
meltErr <- melt(modFitRF$finalModel$err.rate[, classRange + 1], value.name = "Error", varnames = c("Trees", "Classe"))
ggplot(data = meltErr, aes(x = Trees, y = Error, group = Classe, colour = Classe)) +
  geom_line() +
  ggtitle("Number of Trees vs. Error") + 
  geom_vline(xintercept = elbow)

# Normalize the confusion matrix used with the test set, and convert it into a data frame.
cmNorm <- as.data.frame(as.table(apply(cmRF$table, 1, function(x)(x - mean(x)) / sd(x))))

# Set the appripriate columns to the data frame.
colnames(cmNorm) <- c("Prediction", "Reference", "Freq")

# Plot the confusion matrix heat map.
ggplot(cmNorm, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq)) + 
  geom_text(aes(fill = cmNorm$Freq, label = round(cmNorm$Freq, 4)), size = 4) +
  scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) +
  scale_x_discrete(name = "Actual Class") + 
  scale_y_discrete(name = "Predicted Class") + 
  labs(fill = "Normalized\nFrequency") +
  ggtitle("Normalized Confusion Matrix on Test Set")

answers <- predict(modFitRF, eval[, -49])
answers

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

pml_write_files(as.character(answers))



modFitRF$finalModel