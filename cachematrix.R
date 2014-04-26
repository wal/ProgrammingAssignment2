# Create a CacheMatrix, a special Matrix object that can cache its inverse (available by calling getInverse)
makeCacheMatrix <- function(x = matrix()) {

    # Initialize inverse as null (will be calculated on demand)
    inverse <- NULL
    
    # A set function to set the Matrix and reset the cached inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    # Get function to retrieve the Matrix
    get <- function() x

    # Functions to set and get the cached inverse
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse

    # Return list of the wrapped functions of the matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Check if the inverse has already been calculated for this matrix and return it if so, otherwise calculate and cache the inversex
cacheSolve <- function(x, ...) {

    # check if the inverse has been cached already
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("Using Cached inverse")
        # Use the cached inverse
        return(inverse)
    }

    # Inverse not cached, calculate and cache it
    inverse <- solve(x$get(),...)
    x$setInverse(inverse)
    inverse    
}
