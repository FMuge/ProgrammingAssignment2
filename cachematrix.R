## Coursera 
## Course Information: rprog_031 - R Programming
## Assignment # 2
##
## Purpose of the Assignment: Matrix inversion is usually a costly computation and 
## caching the inverse of a matrix rather than compute it repeatedly will be beneficial. 
## 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
    m<-NULL
    set<-function(y) {                                  ##set the value of the matrix
        x<<-y
        m<<-NULL
    }
    
    get <-function() x                                  ##get the value of the matrix
    
    setinversematrix <- function (matrix) m <<-matrix   ##set the value of the inverse matrix
    getinversematrix <- function () m                   ##get the value of the inverse matrix
    list (set = set, get=get, setinversematrix=setinversematrix, 
         getinversematrix=getinversematrix)
}

## This function computes the inverse matrix returned by makeCacheMatrix
## If inverse matrix has already been computed cached computetion is returned 

    cacheSolve <- function (x,...) {
        m <- x$getinversematrix()
        if (!is.null(m)) {
            message("getting cached inverse matrix")
            return (m)
        }
        matrix <- x$get()
        m <- solve(matrix)
        x$setinversematrix(m)
        m
    }
