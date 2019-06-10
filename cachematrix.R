## The functions below set the value for a matrix and its inverse
## Matrix. The matrix object can be cached for assigning new values in
## a new environment.

## makeCacheMatrix() gets a matrix named x as input.

makeCacheMatrix<-function(x=matrix()){
  inverse<-NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(invx) inverse <<- invx
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() returns the inverse value of the matrix x. If object
##x is cached already, the function will return the cached value.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
        ## Return a matrix that is the inverse of 'x'
