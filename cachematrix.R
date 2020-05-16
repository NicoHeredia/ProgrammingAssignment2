## Calculating the inverse of a matrix could be cumbersome. Using the following functions, you can
## do that more efficiently.

## makeCacheMatrix is a function that create a special matrix can cached its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        ivs <- NULL
        set <- function(y) {
                x <<- y
                ivs<<- NULL
        }
        get <- function() x
        setInverse <- function(solveMx) ivs <<- solveMx
        getInverse <- function() ivs
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special matrix created by makeCacheMatrix
## Firstly get the inverse if it was already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ivs <- x$getInverse()
        if(!is.null(ivs)) {
                message("getting cached data")
                return(ivs)
        }
        data <- x$get()
        ivs <- solve(data, ...)
        x$setInverse(ivs)
        ivs
}
