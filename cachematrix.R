## Put comments here that give an overall description of what your
## functions do
## The aim of this assignment is to write a pair of functions -
## "makeCacheMatrix" and "cacheSolve" that cache the revere of a matrix

## Write a short comment describing this function
## The first function makeCacheMatrix creates a special matrix 
## that can cache its reverse for the input (an invertable square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set =set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The second function cacheSolve computes the inverse of the special matrix 
## created by makeCacheMatrix above.
## If the inverse is already calculated, it would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# testing
my_matrix <- makeCacheMatrix(matrix(rnorm(16),4,4))
my_matrix$get()
cacheSolve(my_matrix)





