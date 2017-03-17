## These pair of functions will calculate the inverse of the matrix 
## if it has not been calculated and store it in the cache, or return 
## the cached inverse of the matrix if it has been calculated previously

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setSolve <- function(INV) I <<- INV
        getSolve <- function() I
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        I <- x$getSolve()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setSolve(I)
        I
}