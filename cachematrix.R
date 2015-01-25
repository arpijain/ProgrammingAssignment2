## This assignment is to write a pair of functions that cache the inverse of a
## matrix.There are 2 functions: 

## 1. makeCacheMatrix: This function creates a special "matrix" object that can
##    cache its inverse.

## 2. cacheSolve: This function computes the inverse of the special "matrix"
##    returned by makeCacheMatrix function. If the inverse has already been 
##    calculated (and the matrix has not changed), then the cacheSolve should
##    retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mtrx = matrix()) {

  inv_mtrx <- NULL
  
  set <- function(y) {
    mtrx <<- y
    inv_mtrx <<- NULL
  }
  
  # get the Matrix
  get <- function() mtrx
  
  setInverse <- function(imtrx) inv_mtrx <<- imtrx
  getInverse <- function() inv_mtrx
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the  matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mtrx <- x$getInverse()
  
  if(!is.null(inv_mtrx)) {
    message("Getting cached Inverse Matrix")
    return(inv_mtrx)
  }
  
  mtrx <- x$get()
  inv_mtrx <- solve(mtrx, ...)
  x$setInverse(inv_mtrx)
  inv_mtrx
  
}
