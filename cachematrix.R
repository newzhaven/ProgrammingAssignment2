## cacheSolve() computes the inverse of a matrix
## The result is actually computed for the first time, 
## Then the cached value will be returned for the next calls until a new matrix is set

## Example Usage:
##  x <- makeCacheMatrix(matrix(1:4,2,2))
##  cacheSolve(x)   # compute and cache
##  ...
##  cacheSolve(x)   # return cache
##
##  x$set(matrix(c(1,1,1,3,4,3,3,3,4),3,3))
##  cacheSolve(x)   # compute again


## Return a list of functions used to get/set the matrix and its cached result
## get() - return the matrix
## set() - set the matrix and reset the cache of the inverse
## getsolve() - return the cached inverse
## setsolve() - set the cache (used by cacheSolve() -- should not be called directly)

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return the inverse of matrix 'x' 
## cacheSolve() computes the inverse and cache the result.
## the cached result is returned afterwards until the new matrix is set

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
