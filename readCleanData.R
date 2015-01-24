readCleanData <- function(file, na.rm=TRUE, na.thres=0.1) {
  
  # Read data file, setting blanks to NAs.
  data <- read.csv(file, na.strings=c("NA", ""))
  
  # The vector used to select the clean features.
  cleanFeatures = !logical(ncol(data))
  
  # Perform NA filtering if required.
  if (na.rm) {
    
    # Find all the data set non-NA elements.
    notNA <- !is.na(data)
    
    # Calculate the ratios of NA elements in the columns
    # against the number of observations.
    colRatios <- colSums(notNA) / nrow(data)
    
    # Create the vector of clean features.
    cleanFeatures <- colRatios > na.thres
  }
  
  data[, cleanFeatures]
}