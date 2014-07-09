## Set of functions to calculate and cache the inverse of a matrix
## If the inverse is already stored then retrieve that without recalculating

## prepares a special matrix that can store a matrix and cache its inverse
## 

makeCacheMatrix <- function(x = matrix()) {
  #start with a null inverse, ie it hasn't yet been calculated
  inv <- NULL
  #method to reset the matrix to a new one and clear the cache
  set <- function(y) {
    #check if the matrices are equal and only reset the cache if it's a new matrix being provided by the setter
    if(!identical(x,y)){
    x <<- y
    print("reseting cache")
    inv <<- NULL
  }
  else print('identical matrix not reseting')
  }
  #returns the matrix itself
  get <- function() x
  # set the inverse to the provided value (i)
  setinv <- function(i) inv <<- i
  #retrieve the inverse
  getinv <- function() inv
  #return a list of named functions 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve`  retrieves the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #print(inv)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  #  invert the matrix
  inv <- solve(data, ...)
  #set the inverse
  x$setinv(inv)
  #return the inverse
  inv
}
