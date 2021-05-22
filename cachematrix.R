## This function returns a list of the following objects:
## set - sets the value of the matrix
## get - get the value of matrix
## setinverse - set the inverse of matrix
## getinverse - get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function check the inverse of a particular square matrix
## in the makeCacheMatrix function and returns the inverse,
## otherwise compute the inverse of a square matrix,set the cache inverse
## and returns the inverse of a square matrix

cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
}
