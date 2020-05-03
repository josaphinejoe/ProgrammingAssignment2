## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                in <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) in <<- inverse
        getInverse <- function() in
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        in <- x$getInverse()
        if (!is.null(in)) {
                message("getting cached data")
                return(in)
        }
        mat <- x$get()
        in <- solve(mat, ...)
        x$setInverse(in)
        inv
}
