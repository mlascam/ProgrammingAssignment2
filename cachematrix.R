## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix builds a nxn matrix, then it inverse the matrix

makeCacheMatrix <- function(x = matrix(runif(n^2), nrow = n, ncol = n)) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)


}


## cacheSolve saves the inverse matrix generated in the previous function to save memory and time of computing

cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setinverse(m)
        m
}
