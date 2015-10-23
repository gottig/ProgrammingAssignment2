# Calculate and cache the inverse of a matrix
# makeCacheMatrix creates a special matrix object that can cache its inverse
# cacheSolve computes the inverse of the matrix object returned by makeCacheMatrix


# Create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Inverse of a matrix
  inverse <- NULL
  
  # Set the matrix object
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Get the matrix object
  get <- function() x
  
  # Set the inverse of the matrix object
  setInverse <- function(i) inverse <<- i
  
  # Get the inverse of the matrix object
  getInverse <- function() inverse
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Compute the inverse of the matrix returned by makeCacheMatrix
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  # If the inverse is already computed return the inverse matrix object
  if (!is.null(inverse)) {
    message("Return the cached inverse matrix object")
    return (inverse)
  }
  
  # Inverse is not computed. Compute the inverse matrix object cache it and return the matrix
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
