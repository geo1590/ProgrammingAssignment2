## This script demonstrates how to cache data produced by an algorithm. Caching is needed to avoid
#  re-running potentially long calculations. If the soluion is in the cache, then retrieve and
#  return that solution, otherwise it should calculate it and store the input and output into cache
#  memory.

## This function will return a list of functions used to set/get variables from a different workspace
#  and set/get the matrix inverse. It is used to cache the matrix inverse function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }

    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get=get, setinv = setinv, getinv = getinv)
}


## This function will either calculate the matrix inversion or retrieve the matrix inversion solution
#  from cached memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
             message("getting cached data")
             return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}



