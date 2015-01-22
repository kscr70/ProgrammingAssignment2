## These functions take a square matrix and calculates the inverse 
## of that matrix and stores it in a cache that can be accessed 
## If the original matrix has not been changed. 

## The makeCachematrix funtion creates a list of functions that can
## that can be called on with the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # gives m a null value
        set <- function(y) {    # allows the original matrix to be reset by calling the set function.
                x <<- y
                m <<- NULL
        }
        get <- function() x  # assigns matrix(x) to get
        setInvMatrix <- function(solve) m <<- solve #sets the variable solve to m from the parent environment
        getInvMatrix <- function() m   # r
        list(set = set, get = get,     #prints the list of functions and their environments
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
        
}


## this function calculates the inverse of the matrix from makeCacheMatrix function
## and if it has already been caluculated it retrieves it from memory.

cacheSolve <- function(x, ...) {
        m <- x$getInvMatrix() #assigns m the value of the inverse matrix.
        if(!is.null(m)) {     
                message("getting cached data")  #if the inverse has already been calculated (m not NULL) it gets m from memory
                return(m)                       #and prints it
        }
        data <- x$get()         #gets the matrix x
        m <- solve(data, ...)   #sets m to the inverse of the matrix x
        x$setInvMatrix(m)       #passes m to the setInvMatrix function
        m                       #prints the inverse of matrix x
}
