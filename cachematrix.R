## makeCacheMatrix when passed a matrix will create an object of class list
## of which the elements are 4 functions tied to the matrix passed.
## These functions allow the setting/getting of inverse of the matrix passed

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#cacheSolve when passed an object outputted by makeCacheMatrix (which in turn required an invertible matrix)
#will check to see if it already has a cached value of the inverse, and if not it will calculate the inverse
#and cache the result.

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
}
