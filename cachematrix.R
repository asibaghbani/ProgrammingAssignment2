

## The function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv.matrix <- NULL
  set <- function (y){
    x <<- y
    y <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv.matrix <<- inverse
  getinv <- function() inv.matrix
  list(set = set, get=get, setinv = setinv, getinv=getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
