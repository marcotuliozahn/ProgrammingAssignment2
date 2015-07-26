## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverted_mx <- NULL
       
        set <- function(y) {
                x <<- y
                inverted_mx <<- NULL
        }
        get <- function() x
        set_inv <- function(inv_mx) inverted_mx <<- inv_mx
        get_inv <- function() inverted_mx
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv(m)
        m
}
