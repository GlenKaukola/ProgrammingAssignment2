# makeCacheMatrix function
#
# Input: numeric matrix
# Output: matrix data structure with cached inverse
#
# Description: takes an R matrix and turns it into a data
# structure that has the ability to cache the inverse
makeCacheMatrix <- function(origMatrix = matrix()) {
  invMatrix <- NULL

  set <- function(y) {
    origMatrix <<- y
    invMatrix <<- NULL
  }

  get <- function() origMatrix

  setInverse <- function(incomingInverse) {
    invMatrix <<- incomingInverse
  }

  getInverse <- function() invMatrix

  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve function
#
# Input: data struction created by the makeCacheMatrix function
# Output: numeric
#
# Description: Returns the inverse of a matrix. Caches previously
# calculated inverse to possibly speed up calculations.
cacheSolve <- function(x, ...) {
  # check to see if anything is cached
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached data...")
    inverse
  }

  # nothing is cached
  data <- x$get()  
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
