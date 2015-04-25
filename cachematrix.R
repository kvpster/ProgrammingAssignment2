## These pair of functions calculate and cache the inverse of a matrix
## First create the special matrix, for example: m = makeCacheMatrix(matrix(1:4, 2, 2))
## Then calculate the inverse of this matrix: x <- cacheSolve(m)

## Use this function to create all your matrices that need caching of the inverse calculation

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    list(
        set = function(y) {
            x <<- y
            inverse <<- NULL
        }, 
        get = function() x,
        setinverse = function(inv) inverse <<- inv,
        getinverse = function() inverse
    )
}


## returns the inverse of x, where x was created with makeCacheMatrix

cacheSolve <- function(x) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inverse <- solve(x$get())
    x$setinverse(inverse)
    inverse 
}

