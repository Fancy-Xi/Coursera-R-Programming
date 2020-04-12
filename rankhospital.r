#This is a function that returns the hospital name in a given state based on a given outcome and ranking.
rankhospital <- function(state, outcome, num="best"){
  #Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  outcomes <- c("heart attack","heart failure", "pneumonia")
  states <- data$State
  colnames(data)[c(11,17,23)] <- c("heart attack", "heart failure", "pneumonia")
  data[,c(11,17,23)] = apply(data[,c(11,17,23)],2, suppressWarnings(as.numeric))
  data[,c(2,7)] = apply(data[,c(2,7)],2, suppressWarnings(as.character))
  
  ## check that state and input outcome are valid
  if(outcome %in% outcomes ==FALSE ) {
    stop("invalid outcome")
  }
  if(state %in% states == FALSE) {
    stop("invalid state")
  }
  ## Get data in that state and exclude rows with na in the given outcome
  newdata <- data[data$State==state & !is.na(data[outcome]),]
  ## Rank hospitals based on the name by alphabetic 
  rankname <- newdata[order(newdata$Hospital.Name, decreasing = FALSE),]
  ## Rank hospitals based on a given outcome
  rankoutcome <- rankname[order(rankname[outcome],decreasing = FALSE),]
  rownumber <- nrow(rankoutcome)
  ## Get the name of the selected number of ranking
  if (num == "best") {
    newdata$Hospital.Name[newdata[outcome]== min(newdata[outcome])]
  } else if (num == "worst") {
    newdata$Hospital.Name[newdata[outcome]== max(newdata[outcome])]
  }
  else if (num <=rownumber) {
    rankoutcome[num,]$Hospital.Name
  }
  else {
    "NA"
  }
  
}

#rankhospital("TX", "heart failure",4)
#rankhospital("MD", "heart attack","worst")
#rankhospital("MN", "heart attack",5000)
#rankhospital("NC", "heart attack", "worst")
#rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)