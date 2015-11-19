## These functions create the inverse of a square matrix and cache the result

##This first function has four parts to set and get the value of the matrix
## and set and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
            ##setting up the matrix
            m <- NULL
            set <- function(y) {
                  x <<- y
                  m <<- NULL
            }
            get <- function() x
            ##use the solve function to calculate the inverse of the matrix
            setinverse <- function(solve) m <<- solve
            getinverse <- function() m
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      }


## This second function will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      ##check if the inverse is already cached, and if so, return the cache
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ##if the cache is empty, solve for and return the inverse
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

