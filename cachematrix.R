# Example invocation
# Z <- matrix(c(1,2,3,4),2,2)
# ZC <- makeCacheMatrix(Z)
# cacheSolve(ZC)
# cacheSolve(ZC) # This should show that a cache was used

# makeCacheMatrix: Caches the matrix and inverse results. If matrix value is
# updated, the cache will be wiped of the inverse result
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Clear inverse result cache when setting a new matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Perform inverse of matrix using solve(). If a cached result is available,
# that value is returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse();
  if(!is.null(m)) {
    # Using cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...);
  x$setinverse(m)
  m
}