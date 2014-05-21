## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # This function returns a list of all the below functions
  m <- NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL # since the matrix changed
  }
  
  #Function to get the value of the matrix
  get <- function() x
  
  # Function to set the inverse
  setinv <- function(inv) 
  {
    print("I am in setinv")
    m <<- inv  
  }
  
  # Function to get the inverse
  getinv <- function() m
  
  #set the list
  list(set = set, get = get,
  setinv = setinv,
  getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  #Get the value of global variable m, which contains the cached matrix inverse
  m<-x$getinv()
    
  #Check if cached value is present. If so, return cached value of inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # not cached, so we get the matrix into data
  data <- x$get()
  # and compute the inverse
  inv <- solve(data)
  # then cache the inverse
  x$setinv(inv)
  # and return it as well
  inv
      
}
