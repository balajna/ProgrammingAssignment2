## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # return a list of all the above functions
  m <- NULL
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL # since the matrix changed
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinv <- function(inv) 
  {
    print("I am in setinv")
    m <<- inv  
  }
  # to get the inverse
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
