## These two functions help compute the inverse of a matrix in an
##      efficient way. The computation assumes that the supplied
##      matrix is invertible. Therefore, it will show an error
##      if it is not.

## The inverse of a matrix is saved in the cache the first time it is
##      computed, so it doesn't have to be computed again as long as
##      their values don't change.

## Directions:
## Step 1:
##      A list-type description of the matrix must be created using
##              'makeCacheMatrix' function.
##      'makeCacheMatrix' creates from a matrix a list that
##              $set: sets the values of the matrix
##              $get: gets the values of the matrix
##              $setinv: sets the values of the inverse of the matrix
##              $getinv: gets the values of the inverse of the matrix
##      Usage: makeCacheMatrix(a) # a is a square, invertible matrix
##              Example -first time matrix is defined-:
##                      matrix_list<-makeCacheMatrix(matrix(1:4,2,2))
##              Example -if matrix has already been defined-:
##                      matrix_list$get() # shows values
##                      matrix_list$set(b) # sets matrix values to b
## Step 2:
##      Inverse of the matrix in matrix_list is printed with
##            'cacheSolve' function and stored in the cache.
##      'cacheSolve' manages if the inverse has been computed before
##              and so it recovers the values stored in the cache.
##              It also manages the re-computation of the inverse if
##              the values of the matrix have changed.
##      Usage: cacheSolve(matrix_list) # my_matrix is a list created
##                      from a matrix with makeCacheMatrix
##              Example:
##                      cacheSolve(matrix_list)
##      Additionally, if matrix_list belongs to the class matrix,
##              that is, it hasn't been transformed with
##              makeCacheMatrix, it computes the inverse anyway but
##              the result is not stored in the cache.

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


cacheSolve <- function(x, ...) {
        if(is.matrix(x)) {
                cat("You did not use makeCacheMatrix\n")
                cat("Please type:\n")
                cat("    list_x <- makeCacheMatrix(x)\n")
                cat("if you want to use the cache-efficient\n")
                cat("  process while computing the inverse with:\n")
                cat("    cacheSolve(list_x)\n\n")
                cat("Anyway, I'm computing the inverse for you,\n")
                cat("but the result WILL NOT BE SAVED in cache!!\n")
                listx <- makeCacheMatrix(x)
        } else{listx <- x}
        inv <- listx$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- listx$get()
        inv <- solve(data, ...)
        listx$setinv(inv)
        inv
}
