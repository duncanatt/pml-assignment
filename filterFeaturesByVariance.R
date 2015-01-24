filterFeaturesByVariance <- function(data, var.thres=10) {
  
  # Calculate the variance for each feature.
  featureVar <- lapply(data, var)
  
  # Get the features which have a variance greater than the specified
  # threshold.
  filteredFeatures <- names(featureVar[featureVar > var.thres])
  
  # Filter features.
  data[, filteredFeatures]
}