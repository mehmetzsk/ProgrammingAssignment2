## creates function that holds a list for a few function to keep cache and compute
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  ## set function to store new matrix and set cache to null
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  ## get function returns matris
  get <- function() x
  ## functions sets the cache value
  setinverse <- function(inv) cache <<- inv
  ## gets the cached value
  getinverse <- function() cache
  ## the list returned which keeps functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## get from cache the inverse of compute it and store in cache
cacheSolve <- function(x, ...) {
    cache <- x$getinverse()
    ## if cache not NULL means it was computed before
    if (!is.null(cache)) {
      return(cache)
    }
    ## if cache NULL, get matrix, compute inverse, store inverse in cache and return inverse
    data <- x$get()
    cache <- solve(data)
    x$setinverse(cache)
    cache
}
