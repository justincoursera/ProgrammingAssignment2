## The following functions provide improved performance when repeatedly calling 
## solve() on a matrix. It does so by caching the result so that successive
## calls don't result in re-computation of the inverse matrix.

# Creates a CacheMatrix object
# This stores a matrix, its inverse, and functions to get or set each.
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  
  # return list of functions
  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}

# Cached version of solve()
# If inverse of matrix has already been computed, return cache.
# If not, run solve() and store the result in cache
# Arguments are the same as solve(). At minimum, input a matrix.
cacheSolve <- function(m, ...) {
  
  # attempt get get cached inverse first
  i <- m$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # if unsuccessful, calculate inverse and store it
  data <- m$get()
  i <- solve(data, ...)
  m$setInverse(i)
  i
}
