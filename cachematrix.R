## makeCacheMatrix is a function containing a list of functions to a) set a matrix, b) return the matrix,
## c) set the inverse, and d) get the inverse of the matrix as stored.


makeCacheMatrix <- function(x = matrix()) {
    ## clears the inverse matrix variable
    inv <- NULL
    
    ## sets the primary (user-entered) matrix to x. scopes matrix variable and inv to global environment.
    set <- function (y) {
        x <<- y
        inv <<- NULL
        
    }

    ## shows the matrix x
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
    
    ## returns the cached inverse matrix if it exists
    if (!is.null(inv))  {
        message ("retrieving inverse cached in memory...")
        return (inv)
    }
    
    ## retrieves the matrix using the get() function
    
    data <- x$get()
    inv <- solve (data, ...)
    x$setinv (inv)
    
    
    
    ##returns inv (inverse) of matrix.
    inv
}
