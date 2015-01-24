extractValidFeatures <- function(data, valid.thres=0.1, na.thres=0.1) {
 
  # Get the list of all columns names in the data.
  names = names(data);
  
  # Filter out non-required features by feature name first.
  filterFeatures <- grep("^.*timestampxx.*$", names, value=TRUE)
  
  # Get the list of only those features of interest.
  filteredFeatures <- names[!is.element(names, filterFeatures)]
  
  # The vector containing all those columns we will return as valid.
  validFeatureNames = character(0);
  
  # The total number of observations.
  obsCount = nrow(data)
  
  # Examine each feature.
  for (name in filteredFeatures) {
    
    # Get all observations for the current feature.
    feature = data[, name]
    
    # Extract only the valid observation values for the feature.
    validObs = feature[feature != "" & feature != "#DIV/0!"]
    
    # Calculate ratio for the valid observations against the total
    # number of observations for this feature.
    validRatio = length(validObs) / length(feature)
    
    # Determine whether the current feature qualifies as valid
    # based on the number of valid observations found.
    if (validRatio > valid.thres) {
      
      # Calculate the number of NA values.
      nonNARatio = sum(!is.na(validObs)) / length(validObs)
      
      # Only add the feature if it qualifies as valid based on
      # the number of valid non-NA observation values.
      if (nonNARatio > na.thres) {
        validFeatureNames <- c(validFeatureNames, name)
      }
    }
  }
  
  validFeatureNames
}