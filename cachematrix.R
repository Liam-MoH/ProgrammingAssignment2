## Create a matrix and cache it. Use the cacheSolve function to set the 
## inverse of the matrix. You can then get the inverse from the object 
## without recalculating since it 
## is stored in the cache.

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get, setinverse = setinverse, 
          getinverse = getinverse)
}


## Function that computes the inverse returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
             message("getting cached data")
             return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
