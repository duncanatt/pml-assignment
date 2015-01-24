extractCorrelatedFeatures <- function(data, cor.thres = 0.9) {
  
  # Extract only the numeric features.
  numericFeatures <- sapply(data, is.numeric)
  numericData <- data[, numericFeatures]
  
  # Build the correlation matrix.
  corData <- cor(numericData)
  
  # Since the function cor returns a square matrix, we can
  # use it for both the rows and columns.
  length = seq_along(1:nrow(corData))
  
  # Get the list of matrix column names.
  names <- colnames(corData)
  
  corList = list()
  
  # Loop for all rows and columns, comparing each correlation is
  # above the threhold. Do not consider row-column intersections
  # falling on the leading diagonal.
  for (row in length) {
    
    # Keeps track of the vector of correlated features for this row.
    corFeatures <- character()

    # Compare each feature to the current row.
    for (col in length) {
      if (row != col) {
        if (corData[row, col] >= cor.thres) {
          corFeatures <- c(corFeatures, names[col])
        }
      }
    }
    
    # Only add to the list of correlated features if these were found
    # for current row.
    if (length(corFeatures) > 0) {
      corList[[names[row]]] <- corFeatures
    }
  }
  
  corList
}