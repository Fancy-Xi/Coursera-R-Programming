#this is a function that returns the number of complete cases in a given file
complete <- function(directory, id =1:332) {
  #create a list of files
  allfiles <- list.files(directory, full.names = TRUE)
  #create an empty data frame
  dat <- data.frame()
  for (i in id) {
    #read the files
    readi <- read.csv(allfiles[i],header = TRUE)
    #sum up the complete cases
    nobs <- sum(complete.cases(readi))
    #put the numbers together in a data frame
    dat <- rbind(dat, data.frame(i,nobs))
  }
  colnames(dat)<- c("id","nobs")
  dat
}

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


complete("specdata", 1)
complete("specdata", c(2,4,8,10,12))
complete("specdata", 30:25)
complete("specdata", 3)
