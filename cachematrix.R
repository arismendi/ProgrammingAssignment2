## These functions calculate the inverse of a matrix and saves the result
## in the global environment to avoid calculating over again the inverse matrix of
## previously inverted matrixes.

# Example: source("cachematrix.R")
# x <- matrix(rexp(16), 4) # ramdom values matrix
# list_funcs <- makeCacheMatrix(x) # set matrix x to global environment and create list of functions
# list_funcs$get() # prints the original matrix x values
# cacheSolve(list_funcs) # solve the inverse of x or load data from cache

## This funtions creates a list with the funtions to set/get the values 
## of the original matrix and to set/get its corresponding inverted matrix.
## Original and inverted matrices values are saved in the global environment for
## later use.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #get: returns the original matrix
  get <- function() x
  
  #set: assigns to "inverse" the inverse matrix
  setinverse <- function(inv_matrix) inverse <<- inv_matrix
  
  #getinverse: returns the inverse matrix calculated before if any
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: Calculates the inverse of the matrix and saves the value, but first checks if
## the result has bee calculated (and saved) before. If the matrix is singular it returns an error
## because it can not be inverted.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
