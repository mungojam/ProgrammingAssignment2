## These functions provide a matrix that caches it's inverse, so that
## it can be 'calculated' many times, but is in fact only calculated once
## and only if specifically requested.

## The caching matrix may be used directly through it's $inverse() function
## or the cacheSolve method may be used directly. Both will exhibit the same
## caching behaviour

## This function creates the caching matrix, which can be provided to begin with
## or set later on using the $set() method on the returned matrix object.
## The object returned is a list of the available methods to be called against
## the matrix

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL

    set <- function(y) {
        #Store the new matrix and reset the cache,
        #since it is no longer valid
        x <<- y
        cachedInverse <<- NULL
    }

    get <- function() x

    inverse <- function()
    {
        if(is.null(cachedInverse)) {
            cachedInverse <<- solve(x)
        }
        else {
            message("Using cached matrix inverse")
        }

        return(cachedInverse)
    }

    list(
        set = set,
        get = get,
        inverse = inverse
    )
}


## When given a caching matrix (that caches its inverse),
## this function returns the cached inverse if one exists
## or calculates and caches the inverse before returning
## so that future calls for the same matrix do not need
## to recalculate

cacheSolve <- function(x) {
    #Caching matrix will return cached version of the inverse if available
    return(x$inverse())
}
