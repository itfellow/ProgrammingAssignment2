## Put comments here that give an overall description of what your
## functions do
## this assignment have 2 function
## makeCacheMatrix and cacheSolve

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
# this function accept 


## notes:
# this is my version one , need to update the SHA hash for this file before final grading


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() return(x)
  setsolve <- function(solve) m <<- solve
  getsolve <- function() return(m)
  return(list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve))
  
  
  
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)
  
  
  
}


