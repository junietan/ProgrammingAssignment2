## By.. Junie Tan


makeCacheMatrix <- function(x = matrix()) {
  
  dat <- NULL

  set <- function(y) {
    x <<- y
    dat <<- NULL
  }

  get <- function() x

  setinverse <- function(inData) dat <<- inData

  getinverse <- function() dat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
 
cacheSolve <- function(x, ...) {
  dat <- x$getinverse()
  
 
  if(!is.null(dat)) {
    message("Getting cached data...")
    return(dat)
  }
 
  data <- x$get()
  dat <- solve(data, ...)
  x$setinverse(dat)
  dat
}
