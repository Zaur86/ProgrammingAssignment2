## My functions cache the inverse of matrix.

## First function creates a list of functions, which cache matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        
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


## Second function calculate an inverse of matrix returned by first function. In fact 
        ## this function doesn't calculate the inverse of matrix - it gets the result from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
