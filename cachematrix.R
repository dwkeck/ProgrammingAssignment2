## These functions define and inveert a matrix
## If the inverse has been cached then it is not inverted again

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##      set a matrix
##      get the matrix
##      set the inverse of the matrix
##      get the invere  of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) s <<- inverse
        getinv <- function() s
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function calculates the mean of the special "
## vector" created with the above function. However, it first 
## checks to see if the mean has already been calculated. If so, 
## it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the 
## value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached matrix")
                return(s)
        }
        data <- x$get()
        s <- solver(data, ...)
        x$setinv(s)
        s
}
