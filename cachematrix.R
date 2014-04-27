
## this is a code that compute the inverse of a square matrix can be done with the solve function in R.


## coments: function 1
## in this code this function creates a special "matrix" object that can cache its inverse. it may make sense 
## to cache the inverse matrix so that when we need it again, it can be looked up in the cache rather than recomputed.

makeCacheMatrix <- function(x = matrix())
 {
        m <- NULL
        set <- function(y) 
		{
                x <<- y
                m <<- NULL
        	}
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}




## coments: function 2

## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## Return a matrix that is the inverse of 'x'
