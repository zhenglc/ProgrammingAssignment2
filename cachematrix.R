# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than compute
# it repeatedly. This pair of functions that cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    xinv <- NULL
    
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) xinv <<- inverse
    getinv <- function() xinv
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv )
    
}

# cacheSolve returns the inverse of the matrix. First it checks if the inverse
# has already been computed. If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via setinv function.

cacheSolve <- function(x, ...) {

    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
    
}
