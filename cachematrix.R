## Below are two functions that (1) cache and (2) compute the 
## inverse of a matrix.

## The function immediatetly below creates a matrix object
## that can cache it's inverse.

makeCacheMatrix <- function(y = matrix()) {
  inverse <- NULL
  set <- function(x) {
    y <<- x
    inverse <<- NULL
  }
  get <- function() return(y)
  setinv <- function(inv) inverse <<- inv
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## The function immediatetly below computes the inverse of the
## matrix returned by the function above. If the inverse has
## already been calculated, assuming no change to the matrix, then
## the following function should retrieve the inverse from the cache.

cacheSolve <- function(y, ...) {
  inverse <- y$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  dat <- y$get()
  invserse <- solve(dat, ...)
  y$setinv(inverse)
  return(inverse)
}

