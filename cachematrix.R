## This script can be used to compute inverse of a matrix
## and to cache it in order to save computation time
## when the inverse is being computed repeatedly. 
## The script assumes only invertible matrices as inputs.

## This function creates a special matrix which is able to cache 
## its inverse, so that the inverse does not need to be computed 
## repeatedly. Instead, this matrix allows to fetch the cached 
## result, thus speeding up the computation.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of 'x' and saves the result 
## into cache for future usage. When computing the inverse again
## the cached value is used instead of computing it again.

cacheSolve <- function(x, ...) {  
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
