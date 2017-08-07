## Inversing matrices may take a long time with big data, so these 
## functions are useful in caching matrix data for faster inverse 
## matrix calculation

## This function sets the value of the matrix, and gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function calculates the inverse of the matrix
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
