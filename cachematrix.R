
## makeCache Matrix func caches the invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSove func checks whether the invertible matrix has been cached before or not and assigns value based on that
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  d <- x$get()
  m <- solve(d, ...)
  x$setInverse(m)
  m
}

## TESTING

x = matrix(1:4,2,2)
d = makeCacheMatrix(x)
m$get()
m$getInverse()
cacheSolve(m)