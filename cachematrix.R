## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setInverse <- function(solved) inversed <<- solved
  getInverse <- function() inversed
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inversed <- x$getInverse()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  matrix <- x$get()
  inversed <- solve(matrix, ...)
  x$setInverse(inversed)
  inversed
}
