## Functions to create and cache the inverse of a given matrix



## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # set inv to NULL 
        inv <- NULL
        
        # store matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## if the inverse already exist,then cachesolve retrieves inverse from cache

cacheSolve <- function(x, ...) {
        
        ## get the cached value 
        inv <- x$getInverse()
        
        ## if cached value exist, return it
        if (!is.null(inv)){
                message ("retrieving cached data")
                return(inv)
        }
        ## if cached value does not exist, calculate the inverse and cache it
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
