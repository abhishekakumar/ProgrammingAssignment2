## Abhishek Kumar
## Version: 1.1
## 25 May 2013

## Caching the Inverse of a matrix

## makeCacheMatrix
## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inverse = matrix()) m <<- Inverse
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Return a matrix that is the inverse of 'x'
## If the inverse has already been calculated (and the matrix has not changed), then retrieves the inverse from the cache
## else computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## get the value of inverse from cache
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## inverse not available in cache, computing the inverse
        data <- x$get()
        ## solve(x) computes the inverse of the matrix
        m <- solve(data, ...)
        ## store the computed inverse into cache
        x$setInv(m)
        m
}