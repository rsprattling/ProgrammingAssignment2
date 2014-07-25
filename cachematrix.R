## makeCacheMatrix - Create a matrix-like object that allows caching of its inverse,
##                   using scoping rules to preserve state.
##
## cacheSolve - Return the inverse of a matrix, caching the inverse if it doesn't already
##              exist.

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    ##  Set the matrix data. Note that if matrix is changed (via set fn) then any cached
    ##  inverse is NULLed
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ##  Get the matrix.
    get <- function() x
    
    ## Cache the inverse
    setinverse <- function(i) {
        inverse <<- i  ##Set the inverse.
    }
    
    ## Get the currently cached inverse    
    getinverse <- function() inverse
    
    ## Return the method instances and their implicit data objects.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve - Return the inverse of a matrix, caching the inverse if it doesn't already
##              exist.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    
    ## Meh. No inverse yet exists, so calculate it, set it and return it.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}