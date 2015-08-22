## The function wraps a matrix with the internal state holding its inverse. 
##
## It gets a matrix as input and extends it by some methods:
## - accessor methods for getting and setting the matrix 
## - internal cache for it's inverse with accessor methods to it
makeCacheMatrix <- function(matrix = matrix()) {
  ## initializes the cache
  inverse <- NULL
  
  ## sets the matrix and resets the cache
  setMatrix <- function(m) {
    matrix <<- m
    inverse <<- NULL
  }
  
  ## accessor for the matrix
  getMatrix <- function() matrix 
  
  ## sets the inverse of the matrix into the internal cache
  setInverse <- function(i) inverse <<- i
  
  ## accessor for the inverse
  ## returns NULL if inverse isn't cached
  getInverse <- function() inverse
  
  ## returns the list of wrapper functions to make them accessible
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## The function computes the inverse of a matrix which afterwards will be cached.
## Its input will not be the matrix itself but the wrapped matrix (see makeCacheMatrix() above)
##
## Usage:
## - wrappedMatrix <- makeCacheMatrix(A) (with A is a quadratic matrix)
## - inverse <- cacheSolve(wrappedMatrix)
##
## NOTE: the matrix has to be quadratic, the function doesn't check the preconditions if an inverse can be computed.
cacheSolve <- function(x, ...) {
  ## states if the inverse of the matrix is already cached
  inverseMatrixIsCached <- function(x) {
    !is.null(x$getInverse())
  }
  
  ## computes the inverse of the matrix, puts it into the internal cache and returns it
  computeAndCacheInverseOf <- function(x) {
    ## get the matrix
    matrix <- x$getMatrix()
    
    ## calculate its inverse
    inverse <- solve(matrix)
    
    ## cache it
    x$setInverse(inverse)
    
    ## and return it
    inverse
  }
  
  ## if the inverse is cached than return it immediately
  if(inverseMatrixIsCached(x)) {
    message("getting cached inverse matrix")
    return(x$getInverse())
  }
  
  ## if not compute the inverse, cache it and return it
  computeAndCacheInverseOf(x)
}
