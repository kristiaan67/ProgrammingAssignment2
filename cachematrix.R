##
## Since computing the inverse of a matrix might be a costly computation the R code
## in the file defines functions that created a wrapped instance of a matrix that 
## compute and cache the inverse of that matrix when the inverse is executed the
## first time.
##

#' This function creates a wrapper around a numeric matrix instance that caches 
#' the inverse of that matrix so that the inverse is only computed once.
#' 
#' @param x the matrix instance for which the wrapper is created
#' @return a list containing 4 functions to set and get the internal matrix 
#'         and its cached inverse 
#' 
makeCacheMatrix <- function(x = matrix()) {
    cached_inv <- NULL # the cached inverse
    
    #' sets the internal matrix 'x' to 'y' and resets the cached inverse.
    #' @param y the new internal matrix
    set <- function(y) {
        x <<- y
        cached_inv <<- NULL # reset a possible cached result
    }
    
    #' @return the internal matrix 'x'
    get <- function() { x }
    
    #' Stores (caches) the inverse of the internal matrix 'x'
    #' @param inv the inverse of the internal matrix 'x'
    setInverse <- function(inv) { cached_inv <<- inv }
    
    #' @return the cached inverse of the internal matrix 'x'
    getInverse <- function() { cached_inv }
    
    # return the list with the 4 functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#' This function retrieves the inverse of the wrapped matrix passed as argument 'x'.
#' It first checks whether the inverse has already been computed and cached and
#' if this is the case the cached result is returned. 
#' If not the inverse is computed, cached and returned.
#' 
#' @param x the wrapper instance with the matrix of which the inverse is computed and cached
#' @return the inverse of the wrapped matrix
#' 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # check whether the inverse has already been computed and cached 
    inv <- x$getInverse()
    if (is.null(inv)) { # no cached result yet
        # get the raw matrix,
        data <- x$get()
        # compute its inverse,
        inv <- solve(data, ...)
        # and cache the inverse matrix
        x$setInverse(inv)
      
    } else {
        message("mgetting cached data") # nothing to do
    }
    # finally return the result
    inv
}
