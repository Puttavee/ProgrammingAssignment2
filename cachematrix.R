## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
      }


## The following function invert the matrix created with the above function.
## However, it first checks to see if the inverse has already been done.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the invert of the data and sets the value of the invert in the cache via the set inverse function.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
        ## Return a matrix that is the inverse of 'x'
}

## > x = rbind(c(1, 2), c(3, 4))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## > cacheSolve(m)
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## > 
