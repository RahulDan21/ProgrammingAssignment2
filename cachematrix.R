## Put comments here that give an overall description of what your
## functions do

##1. makeCacheMatrix : Creates a matrix object with access to complete parent env.
##2. cacheSolve : Checks first if inverse is present in cache already, else goes onto
##calculate the same and subsequently store it in cache

## Write a short comment describing this function

##Creates a matrix object for caching
makeCacheMatrix <- function(x = matrix()) {
  
  inv <-NULL
  
  setmatrix <- function(m)
  {
    x <<- m
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(setmatrix=setmatrix, getmatrix=getmatrix,
       setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

## Checks if cache already holds the inverse in memory and aims to retrieve it
## else goes onto compute the same through solve and then store it in cache and
## returns the inverse of the input matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv))
    {
      message("getting cached data")
      return(inv)
    }
    data <-x$getmatrix()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
