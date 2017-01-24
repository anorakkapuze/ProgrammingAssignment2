## The following functions enable computation and caching of 
## a matrix inverse. makeCacheMatrix generates a generalized matrix objecct
## from a matrix input, able to store both the matrix and its inverse,
## while cacheSolve either computes, sets and returns the inverse of the 
## generalized matrix (if it has not been set before), or reads out the 
## matrix inverse from the cache and returns the values (if it has been 
## computed before). 

## This function creates a 'generalized matrix' from a matrix 
## input x. The output has four 'member functions', get and set to
## read out and set the matrix data of mat (respectively),
## and getinv and setinv to read out and set the matrix inverse
## (respectively).

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(mean) matinv <<- mean
  getinv <- function() matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## This function computes the inverse of a matrix mat. 
## It takes as input the 'generalized' matrix x obtained via
## x <- makeCacheMatrix(mat) and either computes its inverse,
## or reads it from the cache (if it has been computed before).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinv(matinv)
  matinv
}
