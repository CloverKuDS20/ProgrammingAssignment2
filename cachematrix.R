## The makeCacheMatrix function creates a list of 4 functions: 
##      set matrix value, get matrix value, solve the inverse matrix, and get the inverse matrix.
## The cashesolve function checks the 4 functions created by makeCacheMatrix for any cached inverse of the input matrix.
##      If a cached value is found, it is returned. If not, cacheSolve computes the inverse matrix and caches it.


## The makeCacheMatrix function takes a matrix and creates a list of 4 functions that:
##  1. Set the matrix. 2. Retrieves the matrix. 3. Set the inverse of the matrix. 4. Retrieves the inverse of the matrix.
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


## cachesolve function computes the inverse of a matrix if it has not been computed and cached by the makeCacheMatrix function.
## Otherwise, the cached inverse is returned.
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
