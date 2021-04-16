## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix produces a list of functions for a matrix 'x' which can be
## individually called using makeCacheMatrix$.

makeCacheMatrix <- function(x = matrix()) {
      n <- NULL
      set <- function(y) {
          x <<- y
          n <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) n <<- solve
      getinverse <- function() n
      list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve searches the list of functions in makeCacheMatrix for already 
## existing solutions to the getinverse function; if that value has not already
## been calculated, then cacheSolve finds the inverse value.

cacheSolve <- function(x, ...) {
     n <- x$getinverse()
     if(!is.null(n)) {
        message("getting cached data")
        return(n)
      }
      data <- x$get()
      n <- solve(data, ...)
      x$setinverse(n)
      n
}
