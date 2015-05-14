## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.


## makeCacheMatrix:
## Set initial inverse to NULL
## set: Specify the matrix, set inverse to NULL
## get: Print the matrix
## setinv: Specify the matrix inverse
## getinv: Print the matrix inverse
## return the special "Matrix" object
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve:
## Load the matrix inverse when it is already cached
## When it is not cached get the data and calculate the inverse
## return the inverse
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
