## These functions cache the inverse of a matrix to avoid repeated computations.

## This function creates a "matrix" object that can cache its inverse.
## Paraphrasing from the assignment description.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function either computes the inverse of the matrix and sets the field in the object in makeCacheMatrix 
## or retrieves the already computed inverse from makeCacheMatrix
## Paraphrasing from the assignment description.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        return(inv)
    }
    matrixData <- x$get()
    inv <- solve(matrixData, ...)
    x$setInverse(inv)
    inv
}
