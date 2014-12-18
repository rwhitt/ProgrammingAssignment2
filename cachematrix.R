## Determines the inverse of a solvable matrix and caches it.
## If the inverse has already been calculated, program returns cashed value


## Creates special "Matrix" - list which contains functions to set and get matrix and inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Checks to see if inverse of makeCacheMatrix "Matrix" has been cached. If yes returns cached inverse
## if no, calculates inverse of matrix

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}