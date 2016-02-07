## Two functions which in combination can create a special matrix that has
## a field to keep a cached version of its inverse self, and also find the
## inverse of a specified inversable matrix

## This function will create a matrix with a storage spot for its own inverse

makeCacheMatrix <- function(x = matrix()) {
      inverse <<- NULL
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function()x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function () inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## This function calculates and returns the inverse of a matrix
## if it finds that there is no cached version of the matrix stored
## in function makeCacheMatrix. If however there is a stored inverse
## it will skip the calculation and just return the stored matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("Getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
