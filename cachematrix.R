##creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(invm) m <<- invm
      getinverse <- function() m
      list(set = set, get = get,
           setinverse=setinverse,
           getinverse=getinverse)
}


## computes the inverse of the matrix created above.  In the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve can retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <-x$get()
      m <- solve(data, ...)
      x$setinverse(m)
}
