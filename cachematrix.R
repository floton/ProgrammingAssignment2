## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## These two functions are used to create a special object that stores a numeric
## matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the stored inverse value to NULL
  inver <- NULL
  
  # set value of the matrix
  set <- function(y) {
    x <<- y
    inver <<- NULL # matrix has changed, reassign to NULL
  }
  
  # get value of matrix
  get <- function() x
  
  # set inverse of matrix
  setinverse <- function(inverse) inver <<- inverse
  
  # get inverse of matrix
  getinverse <- function() inver
  
  # return a list containing all functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
  
  # get inverse
  inv <- x$getinverse()
  
  # if inverse exists, check if already cached
  # if yes, return cached inverse
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  
  # if not, get matrix
  data <- x$get()
  
  # compute inverse of matrix
  inver <- solve(data, ...)
  
  # cache inverse of matrix
  x$setinverse(inver)
  
  # return inverse
  inver
}