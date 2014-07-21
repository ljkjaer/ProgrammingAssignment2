## 'makeCacheMatrix' and 'cacheSolve', create a 'special' matrix, with get and set functions that can cache the 
## inverse of the matrix.'cacheSolve': This function computes the inverse of the special "matrix" returned by makeCacheMatrix' above. 
## If the inverse has ready been calculated (and the matrix has not changed), then 'cacheSolve' should retrieve 
## the inverse from the cache.



## creates a special "matrix", with get and set functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseSolve) m <<- inverseSolve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'. If the inverse matrix has already been calculated, 
## it will be retrived from the cache instead.
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
