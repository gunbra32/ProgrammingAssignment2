## This script provides a function that calculates the inverse of 
## a matrix and caches the result if called for the first time
## or retrieves the cached result if called subsequent times to save computing time
 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
  
  }
  else{
  data <- x$get()
  m <- solve(data, ...)
  message("calculate inverse and cache the result")
  x$setinv(m)
  }
  m
}

# test the code: 

# create sample matrix
omatrix = rbind(c(1, -1/4), c(-1/4, 1))

# first time, the inverse is calculated
cacheSolve(cm)

# the following times, the cached value is used
cm_inv <- cacheSolve(cm)

# check the solution
# the inverse times the original matrix gives the identity matrix
cm_inv %*%cm$get()
