## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function to create a cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                                   # To hold the inverse matrix, and is being set to null,
  
  
  set <- function(y) {                              # To set the value of vector
    x <<- y                                   # Changing the old matrix to the new matrix
    inverse <<- NULL                          # reset the inverse of the matrix of the new matrix.
  }
  get <- function() x                               # To get the actual matrix
  setinverse <- function(solve) inverse <<- solve   # To set the value of the inverse of the matrix
  getinverse <- function() inverse                  # To get the inverse of the matrix
  list(set = set, get = get,                        # To list with the available functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

# The following function calculates the inverse of the matrix" created with (makeCacheMatrix). 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()                     # to holds the inverse of the matrix
  if(!is.null(inverse)) {                       # check if this inverse of the matrix has been calculated
    message("getting cached data")        # if so, prints this message "getting cached data" and
    return(inverse)                       # returns the inverse of the matrix and skips the computation.
  }
  #If the inverse has not been calculated:
  data <- x$get()                               # get the actual matrix       
  inverse <- solve(data, ...)                   # calculating the inverse of the matrix using solve
  x$setinverse(inverse)                         # updating the variable that holds the inverse of the matrix
  inverse                                       
}