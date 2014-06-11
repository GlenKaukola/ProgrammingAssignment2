##########################
# makeCacheMatrix function
##########################
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


#####################
# cacheSolve function
#####################
# Input: data structure created by the makeCacheMatrix function
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

##############
# Testing junk
##############
# simple 2x2
matrix1 <- matrix(c(4, 2, 7, 6), 2, 2)

# 2x2's inverse:
matrix2 <- matrix(c(0.6, -0.2, -0.7, 0.4), 2, 2)

# Make sure it's solving the problem
cacheMatrix <- makeCacheMatrix(matrix1)
solution <- cacheSolve(cacheMatrix)

if (all.equal(solution, matrix2)) {
  message("Getting the right solution is always good")
} else {
  message("Danger Will Robinsion!  We're not getting the correct answer!")
  stopifnot(FALSE)
}

# And check to see if the cache is working
message("It should spit out the message regarding using the cache. Here goes nothing...")
solution2 <- cacheSolve(cacheMatrix)
