## Functions to invert matrix and cache the result
## adapted from the vector/means approach used in the course example

## makeCacheMatrix - defines the necessary functions and returns them in a list

makeCacheMatrix <- function(x = matrix()) {

  inv_matrix <- NULL  #initialise the cached value
  
  get <- function() x  #returns the matrix x that is the argument to the overall function
  setinv <- function(solved) inv_matrix <<- solved  #populates the cache
  getinv <- function() inv_matrix  #will be used to return the cache 
  
  # construct a list containing the three functions
  list(get = get, setinv = setinv, getinv = getinv)
       
}


## cacheSolve - checks to see if there is already a cached inverse for the matrix, 
## returns the cached value if populated, otherwise calculates the inverse (by calling solve),
## stores it and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_matrix <- x$getinv()  #get the currently cached inverse for x
  # if there is a currently cached value then return it
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  # if the current cache was empty then we will continue through these instructions
  data <- x$get()  #get the matrix to invert
  inv_matrix <- solve(data, ...)  #call solve to get the inverse
  x$setinv(inv_matrix)  #cache the result
  inv_matrix  #return the result
  
}
