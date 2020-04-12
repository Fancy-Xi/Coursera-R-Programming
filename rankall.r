#This is a function that returns the hospital name and its state for a given outcome and ranking
rankall <- function(outcome, num="best") {
  #Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  outcomes <- c("heart attack","heart failure", "pneumonia")
  data[,c(11,17,23)] = apply(data[,c(11,17,23)],2, suppressWarnings(as.numeric))
  data[,2]<-as.character(data[,2])
  ## check input outcome are valid
  if(outcome %in% outcomes ==FALSE ) {
    stop("invalid outcome")
  }
  
  ## Find hospital of a given rank for each state
  col <- if (outcome=="heart attack"){
    11
  }else if (outcome=="heart failure"){
    17
  } else {
    23
  }
  
  # data[,col] <- suppressWarnings(as.numeric(levels(data[,col])[data[,col]]))
  # generate an empty vector that can be filled row by row
  output <- vector()
  states <- levels(data[,7])
  for(i in 1:length(states)) {
    statedata <- data[grep(states[i], data$State), ]
    orderdata <- statedata[order(statedata[, col],statedata[, 2],
                                 na.last = NA), ]
    ## Get the name of the selected number of ranking
    hospital <- if(num == "best") {
      orderdata[1,2]
    } else if(num == "worst") {
      orderdata[nrow(orderdata),2]
    }else{
      orderdata[num,2]
    }
    output <- append(output,c(hospital,states[i]))
  }
  ## Return a data frame with the hospital names and state abbr
  output <- as.data.frame(matrix(output, length(states),2,byrow = TRUE))
  colnames(output) <- c("hospital","state")
  rownames(output) <- states
  output
}

#head(rankall("heart attack", 20), 10)
#tail(rankall("pneumonia","worst"),3)
#tail(rankall("heart failure"),10)
#r <- rankall("heart attack", 4)
#as.character(subset(r, state == "HI")$hospital)
#r <- rankall("pneumonia", "worst")
#as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
