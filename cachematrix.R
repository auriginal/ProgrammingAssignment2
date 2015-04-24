## This file contains two functions to store the value of a matrix
## and calculate the inverse efficiently

## An object to store the values of a matrix and its inverse, as well
## as four functions needed to set and retrieve these values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## store new matrix y as x and reset inverse inv
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(newInv) inv <<- newInv
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## A function to calculate the inverse of a CacheMatrix object defined
## by the class above. If the inverse has previously been calculated
## and cached then the cached value is retrieved, otherwise it is
## calculated and cached

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}