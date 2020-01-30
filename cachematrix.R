## Assignment 2
## "makeCacheMatrix" is a shell function that caches the results of matrix inverse
## "cacheSolve" uses the cached results for second and subsequent calculations


makeCacheMatrix <- function(x = matrix()) {
    ## Implement SET, GET, SetInv and GetInv to set a matrix, get a matrix,
    ## store and retreive inverse matrix
    ## 
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}




cacheSolve <- function(x, ...) {
    ## If there is a cached version of the inverse - return it,
    ## otherwise - (a) calculate the inverse of a matrix,
    ##             (b) save the result in the cache
    ##             (c) return the result
    ##
    m <- x$getinv()
    if (!is.null(m)) {
        message("getting cached inverse matrix")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
