## Assignment 2 Solution Reninger
## A pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    # when the matrix changes, we set the cached inverse back to NULL
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list (set = set, get=get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.  If the
## matrix has changed, we recalculate the inverse.

cacheSolve <- function(x, ...) {
  # First, try to get the inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    # if it's not null, give the message below and return the result
      message("getting cached data")
      return(m)
  }
  # if we're here, then it was null, recalculate
  data <- x$get()
  m <- solve(data,...)
  # now set the cached value
  x$setinverse()
  m
}
