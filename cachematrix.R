## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## It is a special "matrix" because returns a list that operates over the matrix
## these operations are: set, get, setsolve and getsolve
## get returns the matrix (NULL if empty)
## set(m) the matrix m will be the matrix cached
## getsolve returns the solve (inverse of matrix) that was previous calculated and cached
## setsolve calculates the solve of the matrix and cache its values
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix returned by the function above
## If the solve matrix has already been calculated returns a message ("getting cached data")
## and without computing the inverse matrix returns the cached inverse matrix
## Otherwise computes the inverse matrix using the above function special "matrix" attributes (get, solve and set)
## example on how to use these functions:
## m = replicate(10, rnorm(10)) 
## cacheSolve(makeCacheMatrix(m)) 
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
  m
}
