## For large matrices, it may take too long to compute the inverse. 
## If you need to do that repeatedly, and the contents of the matrix are not changing, it makes sense to cache the value of the inverse.

## Use this function to set the matrix to be cached. This will be the parameter to put into the next function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,x,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This is the function that will solve your matrix. It will do it only once, and in the next time it is called, it will simply use the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
