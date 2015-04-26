## This is the code submission corresponding for "Programming Assignment 2: Lexical Scoping"
## authored by Fermin Galan Marquez. 

## This file provide two functions (makeCacheMatrix and cacheSolve) to calculate inverse
## matrix in an optimized way using caches. The functions are highly based in the makeVector 
## and cachemean functions shown in the assignment intructions

## In order to test the code you can do the following sequence at the R console:
##
## > source("cachematrix.R")                                        # Load the functions
## > x <- makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0), 3, 3))       # Create an invertible matrix
## > x$get()
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## > cacheSolve(x)                              # Calculate inverse (first time)
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > cacheSolve(x)                              # Calculate inverse again, now using the cache
## getting cached data
##     [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > y <- makeCacheMatrix(cacheSolve(x))        # The inverse of the inverse will result in the original matrix
## > round(cacheSolve(y))
## getting cached data
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0


## This function creates a special "matrix" that can cache its inverse. It is really a list containing a function to
## * set the value of the matrix
## * get the value of the matrix
## * set the value of the inverse matrix
## * get the value of the inverse matrix
## The matrix to create is passed in the 'x' argument as a regular matrix() object
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This functions returns the inverse of a makeCachedMatrix, passed as 'x' argument. The first
## time is invoked on a 'x' makeCachedMatrix, the inverse matrix is calculated and stored in an
## internal cache, so next executions returns the cache directly without re-computing the inverse
## again
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
