## Put comments here that give an overall description of what your
## functions do

## Creates an object capable of caching matrix inverse calculations
##
## Takes a matrix as the one input
##
## Returns list of functions:
##   Item 1: set - sets the value of the matrix
##   Item 2: get - returns the matrix
##   Item 3: setInverse - sets the matrix inverse
##   Item 4: getInverse - returns the value of the matrix inverse
##
makeCacheMatrix <- function(x = matrix()) {

    TheInverse <- NULL
    set <- function(y){
        x <<- y
        TheInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) {TheInverse <<- inverse}
    getInverse <- function() {TheInverse}
    list( set=set, get=get, setInverse=setInverse, getInverse=getInverse )
}


## Solves a matrix for the inverse and return the inverse.
## On the first call, the inverse is calculated and cached.
## On subsequent calls, the cached value is called.

cacheSolve <- function(x, ...) {

    Inverse <- x$getInverse()
    if (!is.null(Inverse)) {
        message("Returning cached data...")
        return (Inverse)
    }
    TheMatrix <- x$get()
    Inverse <- solve(TheMatrix)
    x$setInverse(Inverse)
    return(Inverse)
}
