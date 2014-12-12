## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 

## creates a special matrix 
#returns a list:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize the variable m 
  m <- NULL
  # set the value of the /matrix/ 
  set <- function(y) {
    # set the value of x to input y using <<- 
    x <<- y
    m <<- NULL
  }
  # get the value of the /matrix/
  get <- function() x
  # set the value of the /inverse of the matrix/
  setinv <- function(solve) m <<- solve
  # get the value of the mean 
  getinv <- function() m
  #return a list of 4 numerics
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## define function cacheSolve, takes input from makeCacheMatrix, returns inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  # checks if inv has already been calculated. 
  # If so, gets the mean from the cache 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

