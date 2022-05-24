## makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
##    1.set the value of the matrix
##    2.get the value of the matrix
##    3.set the value of the inversed matrix
##    4.get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(inversed_matrix) inv_matrix <<- inversed_matrix
    getinvmatrix <- function() inv_matrix
    list(set = set, get = get, setinvmatrix = setinvmatrix, 
        getinvmatrix = getinvmatrix)
}


## The following function calculates the inverse matrix of the
## special "matrix" created with the above function.
## However, it first checks to see if the inversed matrix has already been calculated.
## If so, it gets the inversed matrix from the cache and skips the computation. 
## Otherwise, it calculates the inversed matrix of the data and sets the value
## of the inversed matrix in the cache via the setinvmatrix function.

cacheSolve <- function(x, ...) {
    inv_matrix = x$getinvmatrix()
    if(!is.null(inv_matrix)){
        message("getting cached data")
    }
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$setinvmatrix(inv_matrix)
    inv_matrix
}
