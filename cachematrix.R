## Functions makeCacheMatrix to create special matrix that can cache its inverse
## and cacheSolve to retrieve the inverse from an special matrix created by
## makeCacheMatrix.

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


##  Computes the inverse of the special "matrix" returned by makeCacheMatrix
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then cacheSolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) { ## There is no need to check for changes in x,
                          ## because x$set() makes inv <<- NULL
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}

