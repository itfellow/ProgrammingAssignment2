## Put comments here that give an overall description of what your
## functions do
# This project have a pair of functions that cache the inverse of a matrix.

## testing 
# assign makecachematrix to a matrix , eg abcmatrix <- makecachematrix(1:4,2)
# accept pass in matrix via abcmatrix$set(matrix(1:4,3))
# execute the cacheSolve function with the matrix as the input
# additinal: get the identity matrix to show the inverse value are correct


## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  #m represent the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() return(x)
  setInverse <- function(solve) m <<- solve
  getInverse <- function() return(m)
  return(list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse))
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  return(m)
  
  
}


