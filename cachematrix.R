## This function creates a matrix that chaces its inverse

makeCacheMatrix <- function(x = matrix()) {
 cachedInverse <- NULL
    set <- function(newMatrix) {
         x <<- newMatrix
         cachedInverse <<- NULL
     }
    get <- function() {
       x
     }
    setinverse <- function(inverse) {
         cachedInverse <<- inverse
     }    
    getinverse <- function() {
         cachedInverse
     }     
     list(set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of makeCacheMatrix, but only if the inverse has not been cached before

cacheSolve <- function(x, ...) {
     inverse <- x$getinverse()
     if (!is.null(inverse)) {
         return(inverse)
     }
     m <- x$get()
     inverse <- solve(m)
     x$setinverse(inverse)
     inverse
}
