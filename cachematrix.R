## These functions can be used to simplify a long, costly process. Caching a matrix
## can avoid repeatedly solving the same problem.

## This function stores values of a matrix and allows you to set a new value for it 
## as well. It creates a special matrix to set the value of the matrix, get the value 
## of the matrix, set the value of the inverse, and get the value of the inverse.

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

## This function uses a matrix created by the above function to solve the inverse of a matrix.
## If the inverse was already calculated then the function retrieves the inverse from the argument.

cacheSolve <- function(x, ...) {
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
