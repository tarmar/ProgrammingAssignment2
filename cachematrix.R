## The two pair of functions shown here are to help cache the
## inverse of a matrix. 

## Function 1
## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

 ## This is to create a makeCacheMatrix object which will contain
  # a list of four functions to:
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix
  
  # This is originally set to NULL
  # It will change when the user sets the value
  inv <- NULL
  
  # Set function/set matrix
  # Sets the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function/get the matrix
  # Gets the matrix 
  get <- function() x
  
  # Set the inverse of the matrix
  # This will set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse of the matrix
  # This will get the inverse
  getinverse <- function() inv
  
  # Putting them into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}

## Function 2
##The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get the current state of the inverse and check if it has been calculated
  inv <- x$getinverse()
  
  # If this has been done or calculated
  if(!is.null(inv)) {
    # The function will return the calculated inverse		
    message("Getting cached matrix")
    return(inv)
  }
  
  # If not...
  # Get the matrix itself
  data <- x$get()
  
  # Find the inverse
  inv <- solve(data, ...)
  
  # Cache this result in the object
  x$setinverse(inv)
  
  # Return this new result
  inv    
}
