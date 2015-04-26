## This file contains the following two matrix functions that optimize the generation
## of matrix inverses by caching generated inverses and retrieving them from the cache
## if available instead of regenerating them each time.
##
##      makeCacheMatrix - Creates an 'object' that accepts as input a matrix and caches the 
##                        inverse of that matrix. It also contains two functions that insert 
##                        and retrieve the inverted matrix into and from the cache, respectively.
##
##      cacheSolve - Generates the inverse of a matrix using the 'solve' function and stores it 
##                   in the cache the first time executed. On subsequent invocations of this 
##                   function, it retrieves the inverse from the cache.
##
## Test Case:
##      x <- makeCacheMatrix()
##      x$set(matrix(data=rnorm(16), nrow = 4, ncol = 4))
##      x$get()
##      
##      The first call will have to generate the inverse.
##      cacheSolve(x)
##      The second call will retrieve the inverse from the cache.
##      cacheSolve(x)

## Accept a matrix as input. Return a list of functions that cache
## and retrieve the inverted matrix. No error checking is performed
## to insure that the matrix conforms to the requirements of the
## solve() function, i.e. is square, etc.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Accept as input the object (list) created by the makeCacheMatrix$setInverse function.
## If the inverse has already been generated, return the inverse matrix from the cache.
## If not, then solve for the inverse of the matrix and store it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
