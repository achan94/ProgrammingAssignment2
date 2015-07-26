## Below are two functions that are used to create a special
## object that stores a numeric, invertible matrix and cache's its inverse.

## makeCacheMatrix creates a special "matrix", which is a
## list containing a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse, 
## and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function calculates and returns the inverse of the matrix
## generated with the makeCacheMatrix function above. It first
## checks to see if the inverse has been calculated. If so,
## it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x,...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message ("getting cached data")
    return (m)
  }
  data <-  x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
