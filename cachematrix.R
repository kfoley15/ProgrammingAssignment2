## Purpose of program is to create a special "matrix" object which along with storing a matrix,
## also can store it's inverse once calculated once

## makeCacheMatrix creates the "matrix" object and leaves the inverse NULL

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(calculated_inverse) {
    cached_inverse <<- calculated_inverse
  }
  
  getinverse <- function() {
    cached_inverse
  }  
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the object already has a cahced inverse, otherwise it solves 
## for the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
