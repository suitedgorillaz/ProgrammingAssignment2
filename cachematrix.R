## cachematrix.R
## 
## This file contains two functions:
## 
## makeCacheMatrix - creates a 'matrix' with the 'set', 'get', 'setinverse' and 'getinverse' functions. 
## The returned matrix has support for caching the results of solve().

## cacheSolve - tries to get a cached result of a previous solve() call on the object x. If no such result exists, calls the solve method and caches the result in x. 

## Creates a custom 'matrix' that can cache the results of solve() called with the matrix. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL;
    set <- function(y){
        x <<- y
        inv <<- NULL;
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solve with caching. Will check to see if the passed object has cahced the output of a previous solve before executing the solve function.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    
}
