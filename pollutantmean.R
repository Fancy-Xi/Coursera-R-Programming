#this is a function that calculate the mean of pollutant by a given id
pollutantmean <- function(directory, pollutant, id = 1:332) {
  #create a list of files
  allfiles <- list.files(directory, full.names = TRUE)
  #create an empty data frame
  dat <- data.frame()
  #loop through the list of files until id is found
  for (i in id) {
    #read in the file and add to main data frame
    dat <- rbind(dat, read.csv(allfiles[i]))
  }
  # calculate mean of the pollutant and remove NAs
  mean(dat[,pollutant],na.rm = TRUE)
}

pollutantmean("specdata","sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
