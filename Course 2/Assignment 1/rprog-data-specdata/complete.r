complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # Initiate the variable(s)
  data <- data.frame(id = NULL, nobs = NULL)
  
  
  # For each file in the id list, load the data...
  for(i in id) {
    
    # Format the ID to be a fixed width of 3 chars
    fID <- sprintf("%03d", i) 
    fName <- paste(c(directory, "/", fID, ".csv"), collapse = "")
    
    # read and evaluate the contents
    tempData <- read.csv(fName)
    
    # Get the complete cases and count the TRUEs
    comps <- complete.cases(tempData)
    numComps <- nrow(tempData[comps,])
    
    # Add the results to our return frame
    data <- rbind(data, data.frame(id=i, nobs=numComps))
    
  }
  
  # return the results data
  data
}