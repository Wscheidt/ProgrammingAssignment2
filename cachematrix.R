## These two functions take a matrix and then calculates its inverse
## and stores it to the cache

## Takes a Matrix

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
              x <<- y
              m <<- NULL
              }              
              
              get <- function() x
              setinv <- function(inv) m <<- inv
              getinv <- function() m
              list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)
}

## Retrieves the calculated inverse of a matrix from the cache.
## If inverse has not been calculated, the function calculates
## the inverse of the matrix

cacheSolve <- function(x, ...) {
              m <- x$getinv()
              if(!is.null(m)) {
              message("getting cached data")
              return(m)
              }
              data <- x$get()
              m <- solve(data, ...)
              x$setinv(m)
              m 
        ## Return a matrix that is the inverse of 'x'
}
