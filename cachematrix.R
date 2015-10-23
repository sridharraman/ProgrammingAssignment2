## Creates a "special" matrix that holds the original matrix data AND
## caches the inverse of the matrix as well.
## NOTE: It is assumed that the matrix is invertible.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # setter and getter for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # setter and getter for inverse of matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of matrix (if not already cached)
## NOTE: It is assumed that the matrix is invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If inverse cached
  if (!is.null(inv)) {
    message("returning cached inverse")
    return(inv)
  }
  
  # Calculate and set inverse
  matrixdata <- x$get()
  inv <- solve(matrixdata)
  x$setInverse(inv)
  
  return(inv)
}