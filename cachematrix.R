## Put comments here that give an overall description of what your
## functions do
## Function "makeCacheMatrix" creates a special "matrix" 
## object that can cache its inverse. makeCacheMatrix contains 
## 4 functions: set, get, setmean, getmean.

makeCacheMatrix <- function(x = matrix()) {
  in_x <- NULL
  set <- function(y) {
    x <<- y
    in_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) in_x <<-inverse
  getinverse <- function() in_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##Function "cacheSolve" computes the inverse of the special "matrix" 
##(which is the input of cachemean) returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache. If the inverse has not been calculated

CacheSolve <- function(x, ...) {
    r <- x$getinverse()
    if(!is.null(r)) {
      message("getting cached data")
      return(r)
    }
    data <- x$get()
    r <- solve(data, ...)
    x$setinverse(r)
    r
  }
 ## Return a matrix that is the inverse of 'x'

