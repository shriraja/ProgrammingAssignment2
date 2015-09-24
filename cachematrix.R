## The following functions are used to cache the inverse of a matrix
## rather than computing it repeatedly.This will save time and memory.


## This function makeCacheMatrix creates a special matrix object that can cache
## its inverse. It has 2 internal functions to set and get the matrix and
## 2 internal functions to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()
                x
        setinv <- function(inverse)
                inv <<- inverse
        getinv <- function()
                inv
        list(
                set = set, get = get,
                setinv = setinv,
                getinv = getinv
        )
}


## This function cacheSolve computes the inverse of the special matrix returned
## by the above function. If the inverse has already been calculated and matrix 
## has not changed then it returns the inverse from the cache.If the inverse is 
## not in the cache then it computes it and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
