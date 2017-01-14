## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a matrix that can cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix())
{
    in <- NULL
    set <- function(y) {
        x <<- y
        in <<- NULL
    }
    get <- function() x
    setinv <- function(invers) in <<- invers
    getinv <- function() in
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        in <- x$getinv()
        if(!is.null(in)) {
            message("getting cached data")
            return(in)
        }
        data <- x$get()
        in <- solve(data)
        x$setinv(in)
        in
}
