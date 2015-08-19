## This assignment enables to cache 
## time consuming matrix inversion computation

## This function creates an inverse matrix with caching 

makeCacheMatrix <- function(x=matrix()) {
    m<-NULL
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get <-function() x
    setinversematrix <- function (matrix) m <<-matrix
    getinversematrix <- function () m
    list (set = set, get=get, setinversematrix=setinversematrix, 
          getinversematrix=getinversematrix)
}

## This function computes the inverse matrix returned by makeCacheMatrix
## If inverse matrix has already been computes cached computetion is returned 

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
