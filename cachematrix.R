

##This function creates a list of functions from input matrix which is then fed to the cachesolve matrix

makeCacheMatrix <- function(x = matrix()) {
  ## The function will return a list of 4 functions
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x     # To get the data
  setinv <- function(inv) i <<- inv  # Set the inverse of matrix
  getinv <- function() i  #Get inverse, it will be used while checking whether the inverse has been already calculated
  
  #Return a list of functions as an output
  list(set = set, get = get,   # Finally the list of functions being passed as output
       setinv = setinv,
       getinv = getinv)
  
  
}


## This function takes the previous function's output as input

cacheSolve <- function(x, ...) {  # The input will be the output of makeCacheMatrix

  i <- x$getinv()     ## Check if the inverse is existing, then return it

  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)      ## Return from the Cache
  }
  
  ##Else take the matrix data, use solve to calculate inverse 
  data <- x$get()

  i <- solve(data, ...)  ## Calculating inverse
  x$setinv(i)  ## Setting inverse for the future
  
  i  # Return the inverse
  
}
