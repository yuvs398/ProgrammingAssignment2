## The following two functions are being used to inverse an invertible 
## square matrix and cache the inverse of the martix while computing. 

## This function creates a matrix object that can cache its inverse
## for the input for the next function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse)inv <<- inverse
  getinverse <- function ()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cachesolve function computes the inverse of the martix object returned
## by above mentioned function makecacheMatrix. If the invesrse already
## calculated then it will be returned from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("Getting Cached Data")
    return(inv)
  }
  matans <- x$get()
  inv <- solve(matans, ...)
  x$setinverse(inv)
  inv
}
