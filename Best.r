# This is a function that returns the best hospital in a given state for a given outcome.

best <- function(state, outcome) {
  outcomes <- c("heart attack","heart failure", "pneumonia")
  states <- data$State
  ## read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  colnames(data)[c(11,17,23)] <- c("heart attack", "heart failure", "pneumonia")
  data[,c(11,17,23)] = apply(data[,c(11,17,23)],2, suppressWarnings(as.numeric))
  
  ## check that state and input outcome are valid
  if(outcome %in% outcomes ==FALSE ) {
    stop("invalid outcome")
  }
  if(state %in% states == FALSE) {
    stop("invalid state")
  }
  
  ## Get data in that state and find lowest death for a given outcome
  newdata <- data[data$State==state,]
  ## Remove rows with na in the given outcomes
  comdata<- newdata[complete.cases(newdata[outcome]),]
  ## Return hospital name in that state with lowest 30-day death
  comdata$Hospital.Name[comdata[outcome]== min(comdata[outcome])]
}

best("TX","heart attack")
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")


#Alternative approach
best <- function(state, outcome) {
  ## read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  outcomes <- c("heart attack","heart failure", "pneumonia")
  states <- data$State
  colnames(data)[c(11,17,23)] <- c("heart attack", "heart failure", "pneumonia")
  data[,c(11,17,23)] = apply(data[,c(11,17,23)],2, suppressWarnings(as.numeric))
  
  ## check that state and input outcome are valid
  if(outcome %in% outcomes ==FALSE ) {
    stop("invalid outcome")
  }
  if(state %in% states == FALSE) {
    stop("invalid state")
  }
  ## Get data in that state and exclude rows with na in the given outcome
  newdata <- data[data$State==state & !is.na(data[outcome]),]
  ## Return hospital name in that state with lowest 30-day death of the given outcome
  newdata$Hospital.Name[newdata[outcome]== min(newdata[outcome])]
  
}

# best("AL","heart attack")
# best("TX","heart attack")
# best("TX", "heart failure")
# best("MD", "heart attack")
