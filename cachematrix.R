## R-Programming Programming assignment 2
## Author: Jaishree Raman 
## Date: 6/18/2015
## Description: Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  OutputInverseMatrix <- NULL
  
  #initialization setter
  set <- function(y) {
    x <<- y
    OutputInverseMatrix <<- NULL
  }
  
  #Default getter
  get <- function() x
  
  #set Inverse
  setInverse <- function(inverse) OutputInverseMatrix <<- inverse
  
  #get Inverse
  getInverse <- function() OutputInverseMatrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #try to get a previously cached Inverse
  m <- x$getInverse()
  
  #if found, then return the cached Inverse
  if(!is.null(m)) { 
    message("getting cached Inverse Matrix")
    return(m)
  }
  
  #if not found, then call the default getter defined 
  data <- x$get()
  
  #Invert the matrix using solve function. 
  m <- solve(data, ...)
  
  #Now setInverse thereby caching it
  x$setInverse(m)
  
  #return the inverse
  m
  
}


jaiArr <-matrix(1:4,2,2)
solve(jaiArr)

x<-makeCacheMatrix(arr)
cacheSolve(x)