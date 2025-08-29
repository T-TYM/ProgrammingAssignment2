## Caching the Inverse of a Matrix

## makeCacheMatrix:
## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # variable to store the cached inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset cache when a new matrix is set
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## cacheSolve:
## Returns the inverse of the matrix created by makeCacheMatrix.
## If the inverse has already been computed, it retrieves it from the cache.
## Otherwise, it computes the inverse using solve(), stores it in the cache,
## and then returns it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # directly compute the inverse
  x$setinverse(inv)       # store in cache
  inv
}
