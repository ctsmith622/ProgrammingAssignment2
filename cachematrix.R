## The purpose of the functions 'makeCacheMatrix' and 'cacheSolve' are to save time when
##    calculating the inverse of a matrix multiple times. Used together they allow you to create a matrix object, 
##    calculate the inverse, save the inverse, and retrieve the inverse as needed. 
## This means the inverse doesn't need to be recomputed each time it is needed.


## 'makeCacheMatrix' builds a matrix-like object that also contains additional information.
## The matrix-like object contains a matrix object, an inverse matrix object, and 
##    functions to get and set both matrices.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(new_inverse) i <<- new_inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## 'cacheSolve' allows you to calculate the inverse of a makeCacheMatrix object.
## If the makeCacheMatrix object already has a value for its inverse, then 'cacheSolve'
##    simply retrieves the value. Otherwise, it calculates the inverse matrix and assigns
##    the newly calculated matrix to the value of 'i' in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

