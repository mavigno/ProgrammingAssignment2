## This functions are used to solve the inverse matrix and put it in a cache
## allowing to reuse the cached matrix instead of calculating again

## makeCacheMatrix: creates a special matrix with caching abilities
##                  it receives a normal matrix as an argument

makeCacheMatrix <- function(x = matrix()) {
        ## initialize "m" 
        m <- NULL
        ## internal function to setup caching matrix functions
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## define function to get cached matrix
        get <- function() x
        ## define function to set the matrix inverse
        setinverse <- function(solve) m <<- solve
        ## define function to get the inverse of the matrix
        getinverse <- function() m
        ## return list to with special matrix functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function to solve the inverse of a matrix or return it from cache if it was
## alredy solved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## try to get the inverse from the cache
        m <- x$getinverse()
        ## if the matrix is in the cache, get the data and return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if the matrix isn't in the cache, get the data to solve it
        data <- x$get()
        ## try to solve the inverse of the matrix, if its insolvable return an
        ## error
        m <- try(solve(data),silent=TRUE)
        if ("try-error" %in% class(m)) {
                stop("Matrix is not invertible")
        }
        ## store the inverse of the matrix in the cache
        x$setinverse(m)
        ## return the inverse of the matrix
        m
}
