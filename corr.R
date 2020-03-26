#This is a function that calculate the correlation of two pollutant of files that meet the threshold 
corr <- function(directory, threshold=0) {
  #create a list of files
  allfiles <- list.files(directory, full.names = TRUE)
  #create an empty vector
  dat <- vector(mode="numeric", length=0)
  
  for (i in 1:length(allfiles)) {
    #read the files
    readi<- read.csv(allfiles[i], header = TRUE)
    #delete NA and read complete cases
    compcase <- readi[complete.cases(readi),]
    csum <- nrow(compcase)
    # if the number of observations is greater than threshold
    if(csum>threshold){
      #calculate correlation and combine each correlation 
      #for each file in vestor format
      dat<- c(dat, cor(compcase$nitrate, compcase$sulfate))
    }
  }
  dat
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head (cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
