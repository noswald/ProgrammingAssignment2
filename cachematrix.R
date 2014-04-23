## The following two functions help first define a matrix then solve for its
## inverse using a cache to pull from if the inverse has already been solved

## The makeCacheMatrix function creates a "matrix" object that can
## cache its inverse (i)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}


## The cacheSolve function computes the inverse of the matrix returned by
## makeCacheMatrix. If the inverse has been calculated already, it retrieves
## the inverse (i) from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
