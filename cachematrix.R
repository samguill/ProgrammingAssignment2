## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# create a makeCacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(solve) m <<- solve
    
    getInv <- function() m
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


# This function needs library(MASS)
makeCacheMatrix2 <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(ginv) m <<- ginv
    
    getInv <- function() m
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}

## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
    m <- x$getInv()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- ginv(data, ...)
    
    x$setInv(m)
    m
}
