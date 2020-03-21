#determine working derectory
getwd()
#list all the files in working directory
list.files(); dir()
#create a new directory: dir.create("testdir")
#set working directory: setwd("testdir")
#create a filr: file.create("mytest.R")
#check if this file is in working directory: file.exists("mytest.R")
#hange file name:file.rename("mytest.R", "mytest2.R")
#Create a directory in the current working directory called "testdir2" and a subdirectory
 ##for it called "testdir3": dir.create(file.path('testdir2', 'testdir3'), recursive = TRUE)
a<-read.csv("hw1_data.csv")
colnames(a)
head(a)
tail(a)
nrow(a)
b<-a[47,1]
b
#Exclude the NAs, and calculate mean of Ozone
c<-is.na(a$Ozone)
summary(c)
mean(a$Ozone, na.rm=TRUE)
#exrtact the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. Calculate the mean of Solar.R in this subset.
sub1 <- a[which (a$Ozone>31 & a$Temp>90),]
head(sub1)
mean(sub1$Solar.R)  
#calculate mean of Temp when Month is 6
sub2<- a[which (a$Month==6),]
head(sub2)
mean(sub2$Temp)
#calculate maximun ozone value when month is may
sub3<-a[which(a$Month==5),]
head(sub3)
max(sub3$Ozone, na.rm = TRUE)
