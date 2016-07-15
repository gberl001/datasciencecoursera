pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # Initiate the variables
  retVal <- 0
  data <- NULL
  
  # For each file in the id list, load the data...
  for(i in id) {
    # Format the ID to be a fixed width of 3 chars
    fID <- sprintf("%03d", i) 
    fName <- paste(c(directory, "/", fID, ".csv"), collapse = "")
    
    # read and concatenate the contents to our data variable
    tempData <- read.csv(fName)
    data <- rbind(data, tempData)
  }
  
  # Calculate the result
  retVal <- retVal + mean(data[[pollutant]], na.rm = TRUE)
  
  # Return data
  retVal
}