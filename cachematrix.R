## Put comments here that give an overall description of what your
## functions do

## Following function thakes in input a  invertible matrix x
## The function uses the <<- operator in order to  assign a value to an object to be visibile 
## outside the function scope.
## the function create a List of function to get and set matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## if the inverse has already been calculated , the data are get from the cache otherwis the computation is performed.
## function uses the solve() function to compute the inverse of the matrix.

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  if (!is.null(inv)){ 
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  return(inv)
}
