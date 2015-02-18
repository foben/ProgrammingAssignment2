## "makeCacheMatrix" takes a matrix as parameter and 
## creates an object of a matrix whose inverse Matrix can be cached.
## The "cacheSolve" function takes such an object and returns the inverse matrix,
## either by retrieving a cached value or calculating it.

## Create a new cachable matrix from a standard matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    ## If the internal matrix is changed via set,
    ## the cached value is probably incorrect.
    inv_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inv_matrix <<- inv
  getInverse <- function() inv_matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculate the inverse of the supplied matrix,
## returning a cached value if possible
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if ( !is.null(inv) ) {
    message("Returning cached data")
  } else {
    inv <- solve(x$get())
    x$setInverse(inv)
  }
  ## At this point, "inv" contains the correct cached matrix.
  inv
}
