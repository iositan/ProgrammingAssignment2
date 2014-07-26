## Description
# Matrix inversion is usually a costly computation and there may 
# be some benefit to caching the inverse of a matrix rather than
# computing it repeatedly (there are also alternatives to matrix 
# inversion that we will not discuss here). 

# Those are a pair of functions that cache the inverse of a matrix.

## This is the "class" definition in R style

makeCacheMatrix <- function(x = matrix()) {
    # mat_cache is local variable which store the cached matrix (if any)
    mat_cache <- NULL 
    # OOP getter/setter
    # setter will initialize the  matrix and reset the mat_cache
    set <- function(y) {
        # these  are from another environment !
        x <<- y
        mat_cache <<- NULL
    }
    # getter is a simple one line function
    get <- function() x
    
    # here comes the special methods
    setinverse <- function(inverse) mat_cache <<- inverse
    #
    getinverse <- function() mat_cache
    
    # return an object of type list
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## This one is another class
# it will instantiate a class of type makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # note that mat_cache is local here
    mat_cache <- x$getinverse()
    if (!is.null(mat_cache)) {
        message("getting cached data")
        return(mat_cache)
    }
    
    matrix_data <- x$get()
    mat_cache <- solve(matrix_data,...)
    x$setmean(mat_cache)
    mat_cache
    # the above was the return
}
