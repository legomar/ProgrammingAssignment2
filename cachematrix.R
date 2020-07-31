## cachematrix.R is a script that contain two functions that performed the 
##identification of a matrix and its inverse. In addition, it save the result of
##the inverse in order to avoid computing over and over again the same procedure
##ande just provide the value saved in cache.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## This function computes the inverse of the special "matrix" returned by 
##`makeCacheMatrix` above. If the inverse has already been calculated 
##(and the matrix has not changed), then `cacheSolve` should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
