## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object 
# that can cache its inverse
# 1 set the value of the invest

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

# This function compute the inverse of the special matrix 
# by makeCacheMatrix above. If inverse is already
# calculated retrieve from cache

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     if (!is.null(m)) {
          message('getting cached data')
          return(m)
     }
     data <- x$get()
     m <- solve(data,...)
     x$setinv(m)
     m
}

