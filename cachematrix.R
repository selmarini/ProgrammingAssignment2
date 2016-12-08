## Caching the Inverse of a Matrix:
## To reduce the work time it is beneficial to cache the inverse of a matrix rather than computing it when it is needed.
## Function 1 creates a special object that stores a matrix and caches its inverse. 
## Function 2 creates a special object that can cache its inverse.

## Function 1:
makeCacheMatrix <- function(x = matrix()) {  
        inv <- NULL
        set <- function(y) {
                x <<- y    
                inv <<- NULL  
        }  
        get <- function() x  
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv  
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
                
## Function 2:
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }  
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
