## The two functions makeCacheMatrix and cacheSolve work together to create an object
## able to store a matrix and cache it's inverse to improve performance

## Store the matrix and cache inverse value if already calculated
## Return a list of get/set for the matrix and for the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieve inverse from cache or calculate it and cache it into the CacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data = x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
