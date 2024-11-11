## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse property as NULL
  inv <- NULL
  
  # Define a function to set the matrix
  # This function assigns a new matrix value and resets the inverse to NULL
  set <- function(y) {
    x <<- y         # Set the matrix in the parent environment
    inv <<- NULL    # Reset the cached inverse
  }
  
  # Define a function to get the matrix
  get <- function() x  # Simply returns the matrix
  
  # Define a function to set the inverse of the matrix
  # This function caches the inverse in the parent environment
  setInverse <- function(inverse) inv <<- inverse
  
  # Define a function to get the cached inverse, if available
  getInverse <- function() inv  # Returns the cached inverse
  
  # Return a list of the functions defined above
  # This list of functions allows the user to interact with the matrix and cached inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the "matrix" created by makeCacheMatrix.
# If the inverse is already cached, it retrieves the cached inverse.
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse from the special matrix object
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it and print a message
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  # If the inverse is not cached, compute it
  data <- x$get()           # Get the matrix from the object
  inv <- solve(data, ...)   # Calculate the inverse of the matrix
  
  # Cache the inverse for future use
  x$setInverse(inv)
  
  # Return the newly computed inverse
  inv
}
