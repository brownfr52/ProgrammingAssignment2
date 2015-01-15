require(RUnit)
#'
#' Test functions in cachematrix.R
#' 

#'
#' Test function makeCacheMatrix
#' 
test.01.makeCacheMatrix <- function()
{
    # Test no-parameters behavior
    Cached <- makeCacheMatrix()
    m <- Cached$get()
    checkEqualsNumeric(nrow(m),1)
    checkEqualsNumeric(ncol(m),1)
    checkTrue( is.na(m[1,1]) )
    checkTrue( is.null(Cached$getInverse()) )
    
    # Test one-parameter behavior
    set.seed(1)
    Input <- matrix( rnorm(2500,mean=1,sd=2), nrow=50, ncol=50 )
    Cached <- makeCacheMatrix(Input)
    m <- Cached$get()
    checkEqualsNumeric(nrow(m),50)
    checkEqualsNumeric(ncol(m),50)
    checkEqualsNumeric( m, Input )
    checkTrue( is.null(Cached$getInverse()) )
    
    # Test set behavior
    Cached$setInverse(2.5)
    checkEqualsNumeric(2.5,Cached$getInverse())
    Cached$set(Input)
    checkEqualsNumeric( Cached$get(), Input )
    checkTrue( is.null(Cached$getInverse()) )
}

#'
#' Test function cacheSolve
#' 
test.02.cacheSolve <- function()
{
    set.seed(1)
    Input <- matrix( rnorm(250000,mean=1,sd=2), nrow=500, ncol=500 )
    Cached <- makeCacheMatrix(Input)
    Inverse <- solve(Input)
    
    FirstTime <- system.time(First<-cacheSolve(Cached))
    SecondTime <- system.time(Second<-cacheSolve(Cached))
    checkEqualsNumeric( First, Inverse )
    checkEqualsNumeric( First, Second )
    checkEqualsNumeric( FirstTime[3], 0.12, tolerance=0.11 )
    checkEqualsNumeric( SecondTime[3], 0.0, tolerance=0.001 )
}
