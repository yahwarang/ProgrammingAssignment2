## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# default argurment has empty matrix
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    # set null to 'inversed' varilable, because the matrix is renewed.
    inversed <<- NULL
  }
  # return the matrix itself.
  get <- function() x
  # set the 'inversed'
  setInverse <- function(solved) inversed <<- solved
  getInverse <- function() inversed
  # return a list having four elements.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inversed <- x$getInverse()
  # ifthere is a cached....
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  matrix <- x$get()
  # when there is no cache, we need to calculate 'inversed'.
  inversed <- solve(matrix, ...)
  x$setInverse(inversed)
  inversed
}
