## A combination of two functions to allow for caching of result
## from calculating the inverse of a matrix.
## First assign makeCacheMatrix to a value then use $set to "load"
## a matrix. cacheSolve will now calculate the inverse of the matrix
## either by retrieving it in cache or by new calculation

## This function creates a list of four functions allowing the user to
## read and write the matrix and it's inverse to/from cache. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (y) {                             # x is set in the parent environment and can be accessed 
      x <<- y                                         # outside the function scope.
      i <<- NULL                                      # Reset to null since inverse not yet calc. for new matrix
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse     #Function setinverse should never be called (use cacheSolve).
    getinverse <- function() i
    list(set = set,                                   # List giving access to the four functions.
         get = get,
         setinverse = setinverse,
         getinverse = getinverse
         )
}

## This function calculates the inverse of a square matrix (must have been
## set in makeCacheMatrix first). If cache exist, this is returned and
## no calculation is performed. Otherwise computation is made and stored
## in cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()                     # i is a local variable separate from i in function above
    if(!is.null(i)) {                       # i.e. if inverse is stored in cache
        message("getting cached data")
        return(i)
    }
    data <- x$get()                         # otherwise compute inverse ...
    i <- solve(data, ...)
    x$setinverse(i)                         # and store in cache.
    i
}