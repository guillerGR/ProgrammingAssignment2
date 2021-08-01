## Pair of functions that cache the inverse of a matrix.

## Creates a special "matrix" object that can store its own inverse.
## The inverse is cleared if a new input matrix is set.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(result) inverse <<- result
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Takes a special "matrix" object returned by makeCacheMatrix as input. 
## If the input object already has an inverse stored, it returns it. If not, 
## it computes the inverse matrix and stores the result on the input object.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("found inverse in input")
        return(inverse)
    }
    input <- x$get()
    inverse <- solve(input)
    x$setinverse(inverse)
    inverse
}
