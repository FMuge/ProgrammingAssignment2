## Coursera
## Course Information: rprog_031 - R Programming
## Assignment # 2
##
## Purpose of the Assignment: Matrix inversion is usually a costly computation and
## caching the inverse of a matrix rather than compute it repeatedly will be beneficial.
##

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <-
        function(y) {
            ##set the value of the matrix
            x <<- y
            inv <<- NULL
        }
    
    get <-
        function()
            x                     ##get the value of the matrix
    
    setinversematrix <-
        function (matrix)
            inv <<- matrix        ##set the value of the inverse matrix
    getinversematrix <-
        function ()
            inv                   ##get the value of the inverse matrix
    list (
        set = set,
        get = get,
        setinversematrix = setinversematrix,
        getinversematrix = getinversematrix
    )
}

## This function computes the inverse matrix returned by makeCacheMatrix
## If inverse matrix has already been computed cached computation is returned

cacheSolve <- function (x, ...) {
    # Although one of requirements of this assignment states that
    # 'assume that the matrix supplied is always invertible',
    # below conditions are put to prevent error conditions.
    if (dim(x$get())[1] != dim(x$get())[2])  {
        message("This is not a square matrix")
        return()
    } else if (det(x$get()) == 0)  {
        message("This is not an invertable matrix")
        return()
    }
    
    # check for matrix instance whether inverse matrix instance has been cached or not
    inv <- x$getinversematrix()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return (inv)
    }
    
    # cache and return inverse matrix
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinversematrix(inv)
    inv
}
