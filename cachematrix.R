## Implements a special matrix object, suitable for caching inverse matrix

## Provides cache object and methods for getting/setting inverse matrix
## Setter makes cache NULL so when matrix changes, we recalculate.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv ,
             getinv = getinv )
}


## Check if inverse cache already present, if not calculate it and set cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinv()
        if(!is.null(m)) {
             message("Inverse matrix is already cached!")
             return(m)
        }
	  
	  ## No cache present if we reach this line
	  message("Inverse matrix not cached, calculating")
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
