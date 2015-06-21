## File cachematrix.R contains two functions.
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##    retrieve the inverse from the cache.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    ## save inverse results in catche for next time use in variable x
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    ## see if the inverse already exists as cache in variable x
    get <- function() x
    
    ## set value of inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    
    ## get value of inverse matrix
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##  retrieve the inverse from the cache. This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...)
{
    ## see if the inverse already exists by perforing a getinverse()
    inv <- x$getinverse()
    
    ## if the geytinverse() returns a null
    if(!is.null(inv)) {
        message("X has not changed and inverse value is already present.")
        
        ## return the inverse and be done with it
        return(inv)
    }
    
    ## copy x into data
    data <- x$get()
    
    ## calculate the inverse of data and assign results to inv
    inv <- solve(data)
    
    ## save inverse results in catche for next time use
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
