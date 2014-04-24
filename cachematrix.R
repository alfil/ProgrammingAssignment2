## These functions will optimize the calculation of the inverse of a matrix
## saving or caching the result of each calculation for retrieving it
## in case the same operation for the same matrix is demanded

## This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Function to set the value of the original matrix in this object
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## Function to get the value of the original matrix
        get <- function() x
        ## Function to save the inverse of the original matrix
        setsolve <- function(solve) m <<- solve
        ## Function to get the inverse of the original matrix
        getsolve <- function() m
        ## Output of list of functions created by this function
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned
## returned by the function "makeCacheMatrix".
## If the inverse has already been calculated (and the matrix has not changed)
## then this function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## It tries to get the cache value
        m <- x$getsolve()
        ## If it finds it it gets it
        if(!is.null(m)){
                message("getting cache data")
                return(m)
        }
        ## If if does not find it, it calculates it and stores it
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
