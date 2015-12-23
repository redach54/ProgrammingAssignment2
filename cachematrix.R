## These functions creates a matrix and then solves it's inverse. 

## This function is a matrix function. It creates a matrix, then obtains it. 
## In order to prepare for the second function, it is set to make an inverse of the matrix and obtain it.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function is made specifically to get the inverse of a matrix. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

## Making the Matrix
##> x = rbind(c(2, 3), c(3, 2))
##> m = makeCacheMatrix(x)
##> m$get()
##      [,1] [,2]
##[1,]    2    3
##[2,]    3    2

## Solving the Inverse of the Matrix
##> cacheSolve(m)
##     [,1] [,2]
##[1,] -0.4  0.6
##[2,]  0.6 -0.4
