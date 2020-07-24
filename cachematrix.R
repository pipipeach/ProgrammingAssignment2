
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## Below I wrote a pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        ## get teh inverse of the matrix
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}