## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  MyMatrxInverse <- NULL
  set <- function(y) {
    x <<- y
    MyMatrxInverse <<- NULL
  }
  
  get <- function() x
  SetMatrxInverse <- function(inverse) MyMatrxInverse <<- inverse
  GetMatrxInverse <- function() MyMatrxInverse
  list(set=set, get=get, SetMatrxInverse=SetMatrxInverse, GetMatrxInverse=GetMatrxInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# SetMatrxInverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  
  MyMatrxInverse <- x$GetMatrxInverse()
  if(!is.null(MyMatrxInverse)) {
    message("Getting Cached Data. Retrieving Matrix Inverse from Cache.")
    # returning the Matrix that is Inverse of 'x'
    return(MyMatrxInverse)
  }
  
  MatrxDataSet <- x$get()
  MyMatrxInverse <- solve(MatrxDataSet)
  x$SetMatrxInverse(MyMatrxInverse)
  # returning the Matrix that is Inverse of 'x'
  MyMatrxInverse
}

## Sample run:
## > x = rbind(c(1, -1/2), c(-1/2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.50
## [2,] -0.50  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333

## Retrieving from the cache in the second run
## > cacheSolve(m)
##  Getting Cached Data. Retrieving Matrix Inverse from Cache.
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
##

