## This file contains two functions i.e. makeCacheMatrix() and cacheSolve().
## Purpose of these two functions is to cache inverse of a matrix and 
## compute inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.

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

##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been 
##  calculated (and the matrix has not changed), then this function should
##  retrieve the inverse from the cache, otherwise calculate inverse and 
##  finally return the inverse.  

cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data - Inverse of the Matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

