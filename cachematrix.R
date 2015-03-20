## ***
##
## Problem Set 2:
## George F. Dorsey, Jr.
## Coursera - R Programming
## As administered by the Johns Hopkins Bloomberg School of Public Health
## 
## ***
##
## Overview:
## ---------
##     makeCacheMatrix and cacheSolve are functions designed to store a matrix
## and its (lazy-evaluated) inverse for later retrieval in order to avoid costly
## re-computation, for example if the inverse would be calculated multiple times
## within a loop.
##
## Example Usage:
## --------------
##   X <- matrix(c(2,7,17,3,11,19,5,13,23),3,3)  # example matrix
##   X.cached <- makeCacheMatrix(X)              # cache X as a "special" matrix
##   X.inverse <- cacheSolve(X.cached)           # lazy evaluate and get inverse
##
## Credits:
## --------
##     These functions are based on examples provided for caching the mean of a
## vector using the <<- operator to assign to the parent environment.
## Stub file and example (in README.md) originally forked from:
## https://github.com/rdpeng/ProgrammingAssignment2


## makeCacheMatrix essentially creates a stored environment for caching a matrix
## and (if solved) its inverse.
## 
## Argument: x, assumed to be an invertible matrix, (which by definitiion will
## be square) and defaults to an empty matrix if no argument is specified.
##
## Returns: A list of functions which can get or set the original
## matrix or its inverse.  The return list is the intended argument for the
## cacheSolve function described below.  The inverse is initially set to NULL
## and is typically first solved upon the first access to the cacheSolve
## function (i.e. lazy evaluation), though it can also be stored directly using
## the $setInverse function of the returned list if already known.
##
## The returned list includes the functions:
## $set(y)             : Replaces the cached matrix with the matrix represented
##                       by the argument `y`, again assumed to be a square,
##                       invertible matrix, and marks the inverse as unsolved
## $get()              : Retrieves the most recently stored matrix
## $setInverse(inverse): Caches the argument `inverse` as the inverse of the
##                       stored matrix
## $getInverse()       : Retrieves the value cached as the inverse of the
##                       stored matrix, which may be null if not yet set by
##                       $setInverse or cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
    # x is cached by default
    cached.inverse <- NULL
    
    set <- function(y) {
        x <<- y
        cached.inverse <<- NULL
    }
    
    get <- function() return(x)
    
    setInverse <- function(inverse) cached.inverse <<- inverse
    
    getInverse <- function() return(cached.inverse)
    
    return(list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse))
    
}


## cacheSolve takes a "cached matrix" as an argument, that is, a function list
## returned from the function makeCacheMatrix, and returns a matrix that is the
## inverse of the original matrix (assuming this behavior is not modified using
## the ... arguments -- see the solve documentation for details)
##
## Arguments: x, a function list from the makeCacheMatrix method containing
##                an invertible matrix stored within its environment
##            ..., further arguments passed to solve()
##
## Returns: the inverse of the matrix stored in x
##
## Side effects: If the inverse has not yet been calculated, cacheSolve will
##               calculate it and modify x to cache the inverse.  If the inverse
##               has been cached, a message will appear on the console
##               indicating that cached data is being used.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of the matrix cached in 'x'
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # retrieve the cached matrix and store the inverse back in
    # the list environment
    M <- x$get()
    inverse <- solve(M, ...)
    x$setInverse(inverse)
    
    return(inverse)
}
