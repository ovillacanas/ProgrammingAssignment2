## The inverse of a matrix is saved in the cache the first time it is
## computed, so it doesn't have to be computed again
## as long as their values don't change.

## A special description of the matrix must be created
## using makeCacheMatrix.

## Inverse of the matrix is computed with cacheSolve.


## makeCacheMatrix creates from a matrix a list that
## $set: sets the values of the matrix
## $get: gets the values of the matrix
## $setinv: sets the values of the inverse of the matrix
## $getinv: gets the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of the matrix 'x' as long as
## the special list of 'x' has been created with makeCacheMatrix.

## cacheSolve computes the inverse of the matrix 'x' only if it
## hasn't been done before and values haven't changed since last
## computation.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
