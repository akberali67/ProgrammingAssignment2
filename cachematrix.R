## Takes a matrix as input and calculates its inverse, if inverse exists
## in cache then it is taken from there otherwise is recalculated

## creates functions and objects to inverse from cache

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
  
  
  ## Checks the cache if the inverse exists and if not recalculates it
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
           m <- x$getinverse()
           if(!is.null(m)) {
                   message("getting cached data")
                   return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}