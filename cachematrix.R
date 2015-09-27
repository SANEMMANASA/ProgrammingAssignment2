## The objective of this assignment is to cache the inverse of a matrix
## Functions created will create a matrix that allows caching functionality 
## And then cache the inverse of that matrix
## If the matrix is not modified then the cached value of the inverse will be returned, otherwise
## it will be calculated again

# This function accepts a matrix as its parameter and returns a new object 
# that has functions to allow caching

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
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns a matrix that is the inverse of 'x'doing the actual computation 
# and saves it in cache so that it can be used again if required

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
        
}
