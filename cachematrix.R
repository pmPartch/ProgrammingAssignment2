## R Programming Coursera programming assignment 2 for peer review
##
## two functions to support caching of a square matrix inverse
##

## creates a matrix and provides access functions to support cached inverse
##    set(x) will replace the matrix (and null the cache)
##    get() will return the matrix
##    setinverse(y) will cache the matrix inverse
##    getinverse() will return the inverse
## the method returns a list of these functions

makeCacheMatrix <- function(x = matrix()) {
    if (!is.matrix(x))
    {
        #probably should be a stop (Error message)
        warning("requires a matrix as input") 
    }
    
    if (is.matrix(x) && (nrow(x) != ncol(x)))
    {
        #probably should be a stop (Error message)
        warning("matrix must be square in order to calculate inverse") 
    }
    
    m.inverse <- NULL
    set <- function(y) {
        x <<- y
        m.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m.inverse <<- inverse
    getinverse <- function() m.inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## returns a cached matrix inverse or create an inverse and caches it
##
## input to this function is a list provided by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m.inverse <- x$getinverse()
    if (!is.null(m.inverse)) {
        message("getting cached inverse")
        return (m.inverse)
    }
    data <- x$get()
    m.inverse <- solve(data,...)
    x$setinverse(m.inverse)
    m.inverse
}
