## These functions take a square matrix and calculates the inverse 
## of that matrix and stores it in a cache that can be accessed 
## If the original matrix has not been changed. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) m <<- solve
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
