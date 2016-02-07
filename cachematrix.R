##  R Programming Assignment 2
## E. Pillay
  
## This function makeCacheMatrix(x) creates a special "matrix" object that can 
## cache its inverse
##
## x should be a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list is used as the input to the function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {

 inv <- NULL
 set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function cacheSolve(x) computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

## x is the output of the function makeCacheMatrix(). This output is a list
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  # if the inverse has already been calculated simply return the inverse
  if(!is.null(inv)) {
    # get the inverse from the cache and skip the computation. 
    message("getting cached data")
    return(inv)
  }
  # otherwise, go and calculate the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # set the value of the inverse in the cache via the setinverse function.
  x$setinverse(inv)
  inv
}
