## In this assignment I am writing 2 functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## it is a list containing a function to set and get the value of the matrix
## as well as set and get the value of the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverse <<- solve
        getsolve <- function() inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix.If the inverse has already been calculated, then 
##the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getsolve()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
