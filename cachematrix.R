##  Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
## assigns the argument to x
        }
        get <- function() x
## get function returns the matrix
        setInverse <- function(solve) I <<- solve
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
## creates a list of the functions
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
## If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
##Returns the new Inverse value
}
