## Creating a matrix, inversing the matrix and caching the inverse to avoid repeated computation.
## If the inversing is done first time, caching is done. Everytime aftereards if inverse is attempted, 
##first checks if already cached. If yes, retrieve the value from cache.

## MakeCacheMatrix function creates a new matrix object and define functions to 
##set the matrix value, get the matrix value, set the inverse matrix and get inverse matrix from cache.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
      ##set function assign value to matrix object 'x' which is in the parent environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
       ## get function retrieve the value of matrix x
        get <- function() x
        ## Set inv variable(cache ) with the inverse matrix which is in parent env.
        setInverse <- function(inverse) inv <<- inverse
        ##retrieve the cached inverse matrix
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve function checks if the inverse matrix is already avaialble in cache. If so retrives the same without recomputing.
##If not get the inversing done and then store in cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getInverse()
        if (!is.null(inv)) {
                message("retreiving cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
