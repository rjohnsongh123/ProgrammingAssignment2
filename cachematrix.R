## The following functions take adavantage of the R scoping rules 
## to preserve states inside of an R object.

## The function makeCacheMatrix creates a list containing 
## functions that:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix
## This function reminds me of a class with methods and
## properties in C++ or C#

makeCacheMatrix <- function(x = matrix()) {
    ## The inverse of the matrix is NULL until computed
    inv <- NULL
    
    ## Store the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Get the matrix
    get <- function() x
    
    ## Set the inverse
    setinv <- function(inverse) inv <<- inverse
    
    ## Get the inverse
    getinv <- function() inv
    
    ## A list of the functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve will return the inverse of a matrix
## that is stored in "cache". If the inverse is not already computed
## it will be computed when calling this function

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    ## Get the inverse from the cached data if it is
    ## already computed
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Else, compute the inverse of the matrix
    data <- x$get()
    inv <- solve(data)
    
    ## Store in the "cache"
    x$setinv(inv)
    inv
}
