## These functions take a square matrix and calculates the inverse 
## of that matrix and stores it in a cache that can be accessed 
## If the original matrix has not been changed. 

## The makeCachematrix funtion creates a list of functions that can
## that can be called on with the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x  # assigns matrix(x) to get
        setInvMatrix <- function(solve) m <<- solve 
        getInvMatrix <- function() m
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInvMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMatrix(m)
        m
}
