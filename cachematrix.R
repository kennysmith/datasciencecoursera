## Adds methods for caching the inverse of a matrix and retrieving, displaying
## the inverse.

## Usage:
##      m <- m <- matrix(rexp(25, rate=.1), ncol=5)
##      m.cache <- makeCacheMatrix(m)
##      cacheSolve(m.cache)

## Provides methods to save a matrix inverse in cache and return it when asked.

makeCacheMatrix <- function(x = matrix()) {
    
    # Make sure X is a matrix
    if (!is.matrix(x)) {
        stop("Passed value must be matrix")
    }
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Looks for cached inverse of the passed matrix, 
## or computes it if missing. Displays inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

