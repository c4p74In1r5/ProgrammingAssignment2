# Coursera R Programming - Assignment 2
# The following 2 functions demonstrate caching of inverses of matrices
#  - Caching is an expensive operation computationally. These functions help to 
#    reduce the computations by avoiding repeated inverse operations by caching
#    the inverse when it is calculated for the first time
#  - Inverse is calculated by using the builtin solve() function

# The function makeCacheMatrix() returns list of 4 functions for the given matrix x.
#  - set - initializes and caches the matrix x
#  - get - returns the matrix x from cache
#  - setinv - sets the inverse matrix i and caches it
#  - getinv - returns inverse matrix i from cache

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set    = set, 
         get    = get,
         setinv = setinv,
         getinv = getinv)
}


# The function cacheSolve takes the argument x, which is a list returned
# by the function makeCacheMatrix()
#  - It tries to get the inverse of the matrix pointed through x from the cache. If
#    not, it solves for the inverse and caches the inverse

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    
    # Check if inverse is available in cache
    if(!is.null(i)) {
        message("Getting cached data...")
        # If inverse is available in cache, don't compute it again
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
