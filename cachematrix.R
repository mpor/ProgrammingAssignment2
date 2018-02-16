## We want to speed up the inverse matrix computation
## through the usage of the cache

## Creates a vector of functions, 
## to set/get the matrix, and to set/get its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## Check the cache first, if not avaiable: compute
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(x)
  x$setinverse(m)
  m
        
}




