# Create a "matrix" that contains functions to do the following:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize a null inverse matrix 
  i <- NULL
  
  # Initialize an argument matrix and an empty inverse matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Return the argument matrix
  get <- function() {x}
  
  # Set the inverse matrix to the actual computed inverse
  setinv <- function(solve) {i <<- solve}
  
  # Returns the inverse matrix
  getinv <- function() {i}
  
  # Returns a list of all four nested functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks to see if the inverse is in the cache already from a previous
## calculation. If not, it calculates the inverse, puts it in the cache,
## and returns the inverse.

cacheSolve <- function(x, ...) {
  
  # Set the inverse to what is already in the cache
  i <- x$getinv()
  
  # If there is a non-NULL value in the cache, use it and return the inverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # If not, set 'data' to be the original argument matrix
  data <- x$get()
  
  # Calculate the inverse
  i <- solve(data, ...)
  
  # Set the value of the inverse in the cache
  x$setinv(i)
  
  # Return the inverse matrix
  i
}
