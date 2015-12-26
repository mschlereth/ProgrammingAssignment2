# Matrix inversion is usually a costly computation and there may be some benefit to caching
# the inverse of a matrix rather than computing it repeatedly.
#
# The following functions:

# 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#    If the inverse has already been calculated (and the matrix has not changed),
#    then cacheSolve retrieves the inverse from the cache.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
          # Define function to set the value of the matrix. It also clears the old
          # inverse from the cache
          set <- function(y) {
                  x <<- y
                  i <<- NULL
          }
          # Define function to get the value of the matrix
          get <- function() x
          # Define function to set the inverse
          # This is only used by the getinverse() when there is no cache
          setinverse <- function(inverse) i <<- inverse
          # Define function to get the inverse
          getinverse <- function() i
          # stores the 4 functions
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## Checks if value is NOT null pulls from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## The inverse is not yet calculated, so we calculate it
        data <- x$get()
        i <- solve(data)
        # Cache the inverse
        x$setinverse(i)
        # Return the inverse
        i
}
