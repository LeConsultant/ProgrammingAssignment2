## There are 2 functions :
##makeCacheMatrix consists of set,get, setInverse, getInverse
##library(MASS) is used to calculate inverse for non squared as well as square 
##matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                   ##initializing inverse as NULL
    set <- function(y){
       x <<- y
       inv <<- NULL
  }
     get <- function() {x}        ##function to get matrix x
     setInverse <- function(inverse) {inv <<- inverse}
     getInverse <- function() {
                              inver <- ginv(x)
                              inver%*%x   ##function to obtain inverse of matrix
                              }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This is used to get the cache data

cacheSolve <- function(x, ...)    ##gets cache data
  {
     inv <- x$getInverse()
     if(!is.null(inv)){           ##checking whether inverse is null
        message("getting cached data")
        return(inv)               ##returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat,...)           ##calculate inverse value
  x$setInverse(inv)
  inv                             ##return a matrix that is the inverse of x
}
