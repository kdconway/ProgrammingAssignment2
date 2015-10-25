## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function containing a list of functions to a) set a matrix, b) return the matrix,
## c) set the inverse, and d) get the inverse of the matrix as stored.


makeCacheMatrix <- function(x = matrix()) {
    ## set initial state of inv to NULL
    inv <- NULL
    
    ## write matrix to x. sets matrix variable and inv to global environment.
    set <- function (y) {
        x <<- y
        inv <<- NULL
        
    }

    ## return the matrix 
    get <- function () x
    
    ## set inverse of the matrix to inv
    setinv <- function (inverse) inv <<- inverse
    
    ##get inverse of the matrix
    getinv <- function () inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    
    
}

## cacheSolve calculates the inverse of the matrix using the solve() function if inv is set to NULL.
## if previously calculated, cacheSolve returns the stored inverse.

cacheSolve <- function(x, ...) {

    ## checks to see if inv is set or NULL
    
    inv <- x$getinv()
    
    if (!is.null(inv))  {
        message ("retrieving inverse cached in memory...")
        return (inv)
    }
    
    ## retrieves the matrix using the get() function
    
    data <- x$get()
    inv <- solve (data, ...)
    x$setinv (inv)
    inv
    
    
    ##returns inv (inverse) of matrix.
    
}
