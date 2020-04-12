library(datasets)
data(iris)

sub1 <- iris[which(iris$Species=="virginica"),]
sub1
mean(sub1$Sepal.Length)
apply(iris[,1:4],2,mean)

library(datasets)
data(mtcars)
tapply(mtcars$mpg,mtcars$cyl,mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
with(mtcars,tapply(mpg, cyl, mean))

f <- tapply(mtcars$hp,mtcars$cyl,mean)
f

cube <- function(x,n) {
  x^3
}
cube(3)

f <- function(x){
  g<- function(y){
    y+z
  }
  z<- 4
  x+g(x)
}
z<-10
f(3)