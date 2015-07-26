## This programs obtains a Matrix and its Inverse and put them in a list.

## makeCacheMatrix create a list containing a Matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setmatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() inverse
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve keeps in cache the inverse of a entering Matrix and returns its inverse


cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$getmatrix()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
