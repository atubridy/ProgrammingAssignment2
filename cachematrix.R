## The following two functions calculate the inverse of a given matrix. After an
## inverse is calculated it is cached and can be retrieved.

## makeCacheMatrix creates a special matrix that is a list that contains a function to 
## set and get the value and inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ##creates null object, used later
    set <- function(matrix) { ##defines base matrix
        x <<- matrix
        i <<- NULL            ##Creates null inverse matrix, used later
    }
    get <- function() x       ##Retrieves base matrix
    
    setinv <-function(inverse)  i <<- inverse  ##defines inverse
    
    getinv <- function() i    ##Retrieves inverse matrix
    
    list(set = set, get = get,##lists set of objects created
         setinv = setinv,
         getinv = getinv)
}



## cachesolve solves for the inverse of a matrix after it checks to see if the inverse
## has already been cached.

cachesolve <- function (x, ...) {
    i <- x$getinv()                   ##gets inverse if it has already been cacluated
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()                 ##calculates inverse if it does not already exist
    i <- solve(matrix, ...)
    x$setinv(i)                       ##sets inverse so it can be retrieved later
    i                                 ##prints inverse
}


