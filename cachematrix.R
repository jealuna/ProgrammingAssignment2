## This program calculates the inverse of a matrix using the solve
## function in R, the stores in a cache to avoid recomputations. 
## this code is a modification of the example "Caching the Mean of a Vector"
## To run the program we use : 
## y <- makeCacheMatrix(x)
## cacheSolve(y)
## where x is a matrix

## This function creates an object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m<<- solve
    getinverse <- function() m
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computes the inverse of the matrix returned by makeCacheMatrix,
## If the inverse has already been calculated it's retrieved from the cache, else
## the computation of the inverse is done with the solve function in R and stored in
## the cache

cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get ()
    m <- solve(data, ...)
    x$setinverse(m)
    m ## Return a matrix that is the inverse of 'x'
}
