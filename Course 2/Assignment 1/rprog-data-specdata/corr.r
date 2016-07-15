corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Initiate the variable(s)
  retVal <- numeric()
  
  # Get summary of completes
  completes <- complete(directory) # defaults to reading all
  
  # Get subset of data that meets threshold
  interestedData <- completes[completes$nobs > threshold, ]
    
  # For each file in the interested data, get the correlation
  for(i in interestedData[[id]]) {       
    
    # Format the ID to be a fixed width of 3 chars
    fID <- sprintf("%03d", i) 
    fName <- paste(c(directory, "/", fID, ".csv"), collapse = "")
    
    # read and evaluate the contents
    tempData <- read.csv(fName)
    
    # Correlate the (complete cases) data
    compCases <- complete.cases(tempData)
    corVal <- cor(tempData[compCases, ]$sulfate, tempData[compCases, ]$nitrate)
    
    # Add the results to our return frame
    retVal <- c(retVal, corVal)
    
  }
  
  # return the results data
  retVal
}