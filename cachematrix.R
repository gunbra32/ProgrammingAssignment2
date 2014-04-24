## This script provides a function that calculates the inverse of 
## a matrix and caches the result if called for the first time
## or retrieves the cached result if called subsequent times to save computing time
 

## makeCacheMatrix creates a list of 4 function referring to the matrix to be inversed,
## More specifically, the functions
## (1) set the value of the matrix (set()), (2) get the value of the original matrix (get()), 
## (3) calculate and cache the inverse of the matrix (setinv), and (4) get the cached inverse of the matrix (getinv()) 

makeCacheMatrix <- function(x = matrix()) {
  
  # set return value m to NULL at initilization
  m <- NULL
  # define set() function, sets the stored matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # define get function that retrieves the original matrix
  get <- function() x
  # calculate the inverse and cache it in m
  setinv <- function(solve) m <<- solve
  # get the cached value m 
  getinv <- function() m
  # crete the list containing the above functions and return it 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve calculates or retrieves the cached inverse of the original matrix and returns it
cacheSolve <- function(x, ...) {
  # get the inverse of x; NULL if called for the first time
  m <- x$getinv()
  # if m is not NULL it is the cached value
  if(!is.null(m)) {
    # print what happens
    message("getting cached data")
  }
  # if m is NULL
  else{
  # get the original matrix
  data <- x$get()
  # calculate the inverse using solve()
  m <- solve(data, ...)
  # print what happens..
  message("calculate inverse and cache the result")
  # cache the inverse matrix using the setinv() function
  x$setinv(m)
  }
  # return m
  m
}

# test the code: 

# create sample matrix
omatrix = rbind(c(1, -1/4), c(-1/4, 1))

# create the list of functions
cm <- makeCacheMatrix(omatrix) 

# first time, the inverse is calculated
cacheSolve(cm)

# the following times, the cached value is used
cm_inv <- cacheSolve(cm)

# check the solution
# the inverse times the original matrix gives the identity matrix
cm_inv %*%cm$get()

# create new matrix using the set() function
cm$set(rbind(c(1, -1/2), c(-1/2, 1)))
# invert the new matrix
cm_inv <- cacheSolve(cm)
