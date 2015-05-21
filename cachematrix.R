## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse.
# The funcation create a list that containing the function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of matrix inverse
# 4. get the value of matrix inverse
## 
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


## Comute the inverse of the target matrix
# The function check if the inverse result already cached. If the inverse had already been
# cached, return the cache and skip the computation.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
