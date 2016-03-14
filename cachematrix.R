## The funtions makeCacheMatrix and cacheSolve are able to calculate the 
## inverse of a matrix and use the result as a cached value if the same
## input is supplied to the function. 

## The function makeChacheMatrix creates a list containing four functions:
## set: Set the value of the matrix
## get: Get the value of the matrix
## setinv: Set the value of the inverse
## getinv: Get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cachesolve calculates the inverse of a matrix using the function
## "makeCacheMatrix". Before calculating the value it checks wether the value
## has already been calculated. If so the value is retrieved from cache. The 
## newly calculated value is stored in cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
               return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv     ## Return a matrix that is the inverse of 'x'
}
