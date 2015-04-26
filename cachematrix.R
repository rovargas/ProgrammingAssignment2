## Coursera R Programming Course
## Programming Assignment 2

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(m) {
        x <<- m
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    m <- x$get()
    i <- solve(m)
    x$setInverse(i)
    i
}
