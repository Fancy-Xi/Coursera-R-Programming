set.seed(1)
rpois(5, 2)
set.seed(10)
x <- rep(0:1, each = 5)
x
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)

data <- read.csv("outcome-of-care-measures.csv")
levels(data[,7])
colnames(data)
colnames(data)[c(11,17,23)] = c("heart attack", "heart failure", "pneumonia")
colnames(data)

states <- data[,7]
head(states)
unique(states)

which.min(data$`heart attack`)

library(data.table)
data <- read.csv("outcome-of-care-measures.csv")
#remove rows that has na in "heart attack" column
comdata<- newdata[complete.cases(newdata[,11]),]
DT <- data.table(comdata)
DT[,.SD[which.min(data$`heart attack`)], by = data$State]

#select out the data for a given state
data <- read.csv("outcome-of-care-measures.csv")
colnames(data)[c(11,17,23)] <- c("heart attack", "heart failure", "pneumonia")
newdata <- data[which(data$State=="TX"),]
newdata[,c(11,17,23)] = apply(newdata[,c(11,17,23)],2, as.numeric)
head(newdata)
#remove rows that has na in "heart attack" column
comdata<- newdata[complete.cases(newdata[,11]),]
head(comdata)
#select out the minimun death for heart attack
min(comdata$`heart attack`)
comdata$Provider.Number[comdata$`heart attack`== min(comdata$`heart attack`)]
comdata$Hospital.Name[comdata$`heart attack`== min(comdata$`heart attack`)]


#rank hospitals based on the given outcome, "order" gives the position of the items
hospitalrank <- order(comdata$Hospital.Name, decreasing = FALSE)

hospitalrank <- order(hospitalrank$`heart failure`, decreasing = FALSE)

# the following command sort hospitals and output the whole dataset, not the position
data[,c(2,7)] = apply(data[,c(2,7)],2, as.character)
class(data[,7])
data[,c(11,17,23)] = apply(data[,c(11,17,23)],2, suppressWarnings(as.numeric))
class(data[,11])
rankname <- comdata[order(comdata$Hospital.Name, decreasing = FALSE),]
head(rankname$Hospital.Name)
rankoutcome <- rankname[order(rankname$`heart failure`, decreasing = FALSE),]
head(rankoutcome)
#give the name of that hospital with the given ranking
numb <- c(1,2,3,4)
#select out the rows of targeted hospital
rankoutcomes <- data.table(rankoutcome)
head(rankoutcomes)
#output the hospital name and state
output <- rankoutcome[numb,][,c(2,7)]
colnames(output) <- c("hospital","state")
output

newdata <- data[ !is.na(data$`heart attack`),]

output <- data.frame()
for (i in 1:length(states)) {

}

head(states)
str(states)
head(data[grep(states[2],rankoutcomes$State),])
s<- matrix(1:12,4,6)
s
s[25]

statedata <- data[grep(states[1],data$State),]
head(statedata)

num <- 10
for (i in 1:length(states)) {
  statedata <- data[grep(states[i],data$State),]
  orderdata <- statedata[order(statedata[,col],statedata[,2],
                               na.last = NA),]
  ## Get the name of the selected number of ranking
  hospital <- if (num == "best") {
    orderdata[1,2]
  } else if (num == "worst") {
    orderdata[nrow(orderdata),2]
  }
  else {
    orderdata[num,2]
  }
  output <- append(output,c(hospital,states[i]))
}
  
  data <- read.csv("outcome-of-care-measures.csv")
  colnames(data)[c(11,17,23)] <- c("heart attack", "heart failure", "pneumonia")
  states <- levels(data[,7])
  length(states)








