## The following functions are used to cache the inverse matrix of an invertible
## matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  imat <- NULL
  set <- function(y){
    x <<- y
    imat <<- NULL
  }
  get <- function() {x}
  set_inverse <- function(inverse) {imat <<- inverse}
  get_inverse <- function() {imat}
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  imat <- x$get_inverse()
  if(!is.null(imat)){
    message("getting cached data")
    return(imat)
  }
  mat <- x$get()
  imat <- solve(mat,...)
  x$set_inverse(imat)
  imat
}
