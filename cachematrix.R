## The following two functions are used to calculate the inverse of a matrix. 
## If the matrix has been cached before, the inverse of that matrix will be retrieved 
## from the cache and returned by the function rather than repeating the inverse computation
## repeatedly.

## Function 1 - makeCacheMatrix()
## This function creates a 'special matrix' which is basically a list of functions to -
##     1. set value of the matrix
##     2. get value of the matrix
##     3. set inverse of the matrix
##     4. get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function 2 - cacheSolve()
## This function calculates the inverse of the special "matrix" created
## with the above function. Before calculating, it first checks to see if the inverse of the 
## matrix has already been calculated. If that is the case, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the data and sets
## the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
