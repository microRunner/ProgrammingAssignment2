

##This function creates a list of functions from input matrix which is then fed to the cachesolve matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  #Return a list of functions as an output
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## This function takes the previous function's output as input

cacheSolve <- function(x, ...) {

  i <- x$getinv()
  
  ## Check if the inverse is existing, then return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ##Else take the matrix data, use solve to calculate inverse 
  data <- x$get()

  i <- solve(data, ...)
  x$setinv(i)
  # Return the inverse
  i
  
}
