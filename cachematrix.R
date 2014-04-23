##
## cacheMatrix:
##
## wrapper object for cached matrix inverse
##
## usage:
##
## cmat <- makeCacheMatrix(mat) creates a new cacheMatrix cmat from an R matrix mat
## mat <- cmat$get() recovers the original R matrix
## cacheSolve(cmat, ...) works the same as solve(mat, ...) with benefit of caching
## cmat$setinverse(NULL) empties the cache
##
## WARNING - second and subsequent calls to cacheSolve will return the same result as the first call
##         - this will be the same whatever additional arguments are supplied after the first call
##         - unless the cache is emptied using setinverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize cached inverse m to NULL - not calculated yet
    m <- NULL
    # internal copy of R matrix is stored in x - when x is set cached inverse is reset to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # function to retrieve R matrix
    get <- function() x
    # set value of cached inverse
    setinverse <- function(inverse) m <<- inverse
    # return value of cached inverse
    getinverse <- function() m
    # cacheMatrix is a list of functions - getters and setters for matrix and its inverse respectively
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: solve(x, ...) function where x has been created using makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Query x for cached inverse
        m <- x$getinverse()
        if(!is.null(m)) {
            # cached inverse exists
            message("getting cached data")
            return(m)
        }
        # cached inverse does not exist, so run solve on R matrix inside cacheMatrix
        data <- x$get()
        m <- solve(data, ...)
        # place inverse in cache before returning it
        x$setinverse(m)
        m
}
