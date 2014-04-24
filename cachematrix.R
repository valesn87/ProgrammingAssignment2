## This function creates a matrix that chaces its inverse

makeCacheMatrix <- function(x = matrix()) {
 cachedInv <- NULL
    set <- function(matrix2) {
         x <<- matrix2
         cachedInv <<- NULL
     }
    get <- function() {
       x
     }
    setinverse <- function(inverse) {
         cachedInv <<- inverse
     }    
    getinverse <- function() {
         cachedInv
     }     
     list(set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of makeCacheMatrix, but only if the inverse has not been cached before

cacheSolve <- function(x, ...) {
     inverse <- x$getinverse()
     ##the following lines check if the inverse is already cached
     if (!is.null(inverse)) {
         return(inverse)
     }
     a <- x$get()
     inverse <- solve(a)
     x$setinverse(inverse)
     inverse
}
