# The two functions makeCacheMatrix and cacheSolve implement
# a pair of funtions that allow to cache the inverse of the matrix
# this might be useful as the computation of the inverse of a matrix
# is quite expensive.

# makeCacheMatrix creates a cached matrix object of an
# ordinary matrix
#
# example: 
#   cachedmatrix = makeCacheMatrix(matrix(c(12,30,43,99), 2, 2))
# 

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize cache for inverse
    i <- NULL
    
    # setter for matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # getter for matrix
    get <- function() {
        x
    }
    
    # setter for inverse
    setInverse <- function(inverse_m){
        i <<- inverse_m
    }
    
    # getter for inverse
    getInverse <- function() {
        i
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve is a function that takes a cached matrix,
# checkes the cache for an already available inverse.
# If the inverse of the matrix has already been
# calculated, it just returns this inverse.
# Otherwise, it calculates the inverse, updates
# the cache and finally returns the inverse.
#
# example:
#   cachedmatrix = makeCacheMatrix(matrix(c(12,30,43,99), 2, 2))
#   cacheSolve(cachedmatrix) # this time it calculates the inverse...
#   cacheSolve(cachedmatrix) # this time it takes the inverse of the cache

cacheSolve <- function(x, ...) {
    # fetch the available cache
    i <- x$getInverse()
    
    # check the available cache,
    # if != NULL just return it
    if (!is.null(i)){
        message("getting cached inverse")
        return (i)
    }

    # calculate inverse
    inverse <- solve(x$get())
    
    # update cache
    x$setInverse(inverse)
    
    # return the inverse
    inverse
}
