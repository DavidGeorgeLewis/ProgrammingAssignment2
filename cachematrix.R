## Calculating an inverse of a matrix

## These functions create the inverse of matrix (x).  However the
## calculation of a matrix's inverse is expensive computationally
## and therefore if the matrix's inverse has been previously
## calculated the inverse is drawn from the cache rather than being 
## calculated again.

## "makeCachematrix" has a cache for the inverse of the
## matrix x.  Before the inverse is calculated (in the function "cache 
## solve") the matrix in the cache = NULL.  When the inverse is 
## calculated it is entered in the cache "m" and supplied back to 
## "cacheSolve" through the function "getinverse"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Returns a matrix that is the inverse of 'x' created 
## with the solve() function. However, it first checks to see 
## if the inverse has already been calculated. If so, it 
## gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of x and sets the inverse
## in the cache via the setinverse() function.


cacheSolve <- function(x, ...) {
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