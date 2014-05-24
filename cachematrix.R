## These functions create and invert a matrix
## If the inverse has been cached then it is not inverted again

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) s <<- inverse
        getinv <- function() s
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
##  been calculated (and the matrix has not changed), then the 
## cachesolve  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached matrix")
                return(s)
        }
        mat <- x$get()
        s <- solver(mat, ...)
        x$setinv(s)
        s
}
