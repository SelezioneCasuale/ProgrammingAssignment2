## These two functions calculate and store the inverse of a given square matrix. 
## If tsuch inverse matrix has already been calculated, it is retrievd from cache 
## and computation is skipped. 


## The makeCacheMatrix function allows to:
## i - set the value of the matrix
## ii - get the valure of the matrix
## iii - set the value of the matrix inverse
## iv  - get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## the following function calculates the inverse of the matrix
## created with the above function; if the inverse has already been calculated, 
## it gets it from the cache and skips the computation. Otherwise, it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
