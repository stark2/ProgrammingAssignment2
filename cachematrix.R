## Introduction
# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching inverse of a matrix 
# rather than compute it repeatedly. 
# Assumption: the matrix supplied is always invertible. 

## makeCacheMatrix 
# contains a list of functions to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## cacheSolve
# returns the inverse of a matrix. However, it first checks to see 
# if the inverse of the matrix has already been calculated. If so, 
# it gets the values from the cache and skips the computation. 
# Otherwise, it calculates the inverse and sets the value 
# in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


## Test:
# > x <- matrix(1:4, 2, 2)
# > m = makeCacheMatrix(x)
# > m$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data
# [1,]   -2  1.5
# [2,]    1 -0.5
