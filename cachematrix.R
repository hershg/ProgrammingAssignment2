# R Programming - Programming Assignment 2 - Lexical Scoping

# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
  
  # holds cached value or NULL (if nothing is cached)
  i <- NULL
  
  # set value of matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get value of matrix
  get <- function() x
  
  # set value of inverse
  setInverse <- function(inverse) i <<- inverse
  
  # get value of inverse
  getInverse <- function() i
  
  # return list containing function methods
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


# cacheSolve: This function calculates the inverse of a 
# special "matrix" object created with makeCacheMatrix.
# If the cache already exists, it will return cached value.
cacheSolve <- function(x, ...) {
  # get cached value
  i <- x$getInverse()
  # if cache already exists, return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # otherwise caclulate the inverse (and cache it)
  data <- x$get()
  i <- solve(data) # Calculate inverse
  x$setInverse(i)
  
  # return inverse
  i
}