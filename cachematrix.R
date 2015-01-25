## The below functions are used to cache the inverse of a given matrix.
## It will calculate the matrix's inverse if the cache is empty

## This function sets and retrieves the matrix passed into the function.
## It also caches the inverse of a matrix

makeCacheMatrix <- function(m = matrix()) {
  
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of a matrix if not done already.
## If an inverse of the matrix exists already, it returns the same.

cacheSolve <- function(m, ...) {
  
  ## Return a matrix that is the inverse of 'm'
  i <- m$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data)
  m$setInverse(i)
  i
  
}