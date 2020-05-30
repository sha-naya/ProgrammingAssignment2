## Author: sha-naya
## Date: 30/05/2020
## Description: This .R file contains two functions, makeCacheMatrix and cacheSolve.
## They are based on the provided functions of Week 3 Programming Assignment 2 R Programming
## Coursera course. This file was originally forked from rdpeng on GitHub.

## This function creates a list that contains four functions that set and store the matrix, 
## then set and get the value for its inverse. In plain language, it caches the "matrix".
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  set.inv <- function(inverse) i <<- inverse
  get.inv <- function() i
  
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}

## This function computes the inverse of the "matrix" returned by the previous function. It also
## checks if the inverse has already been calculated and if it has not changed. This is done so
## that the inverse can be simply returned from cache and not re-calculated unnecessarily. 
cacheSolve <- function(x, ...) {
  i <- x$get.inv()
  if(!is.null(i)) {
    message("Getting cached data. Please wait.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set.inv(i)
  i
}