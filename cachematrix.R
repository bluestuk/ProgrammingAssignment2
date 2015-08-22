## Functions to construct and manipulate a cache matrix.

## A cache matrix encapsulates a matrix.  The inverse of that matrix is
## cached when first solved to allow efficient retrieval of the inverse on
## subsequent calls to solve.

## Construct a cache matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse
    )
}


## Find the inverse of a cache matrix.  Assumes x is invertible.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
