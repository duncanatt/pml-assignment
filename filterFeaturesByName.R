filterFeaturesByName <- function(data, names=c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window", "total_accel_belt", "total_accel_arm", "total_accel_dumbbell", "total_accel_forearm")) {
 
  # Get the list of feature names in the data.
  featureNames = names(data)
  
  # Get the list of the features to be included in the data set.
  filteredFeatures <- featureNames[!is.element(featureNames, names)]
  
  # Filter features.
  data[, filteredFeatures]
}