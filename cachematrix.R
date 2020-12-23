## makeCacheMatrix is a function to create a matrix and cacheSolve returns 
## the inverse matrix

## makeCacheMatrix would create a square matrix used for the next conjunction

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve returns the inverse matrix stablished earlier and
## stores it when getInverse asked

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}