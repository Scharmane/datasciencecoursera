#"Assignment: Caching the Inverse of a Matrix"
# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly". 
# MakeCacheMatrix will store the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  inverse_value <- NULL
  set <- function(y) {
    x <<- y
    inverse_value <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse_value <<- inverse
  getInverse <- function() inverse_value
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# CacheSolve will computes the inverse of the matrix created by 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return the inversed matrix of 'x'
  inverse_value <- x$getInverse()
  if (!is.null(inverse_value)) {
    message("found cached data")
    return(inverse_value)
  }
  mat <- x$get()
  inverse_value <- solve(mat, ...)
  x$setInverse(inverse_value)
  inverse_value
}
