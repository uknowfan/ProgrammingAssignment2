## in the coding below, I created two functions makeCacheMatrix and cacheSolve. 
## They are used for matrix inversion. If the matrix inversion is a repeated 
## process for a matrix, then my function will cache the inverse matrix instead
## of computing it again



## makeCacheMatrix is used to create a list which contains a matirx and 
## caches its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        r <- list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve is used to compute matrix inversion. If the computation was
## done before, it will retrive the cached inverse matrix instead of computing 
## it again. 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m 
        ## Return a matrix that is the inverse of "x"
}
