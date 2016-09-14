## Put comments here that give an overall description of what your
## functions do: : This function is to calculate the inverse of data in a matrix.

## Write a short comment describing this function: 
##(1)set a matrix.
##(2)get is a function returns the value of the matrix stored in the main function.
##(3)setinverse and getinverse are functions very similar to set and get.
##(4)They don’t calculate the inverse, they simply store the value of the input in a variable m.
##(5)into the main function makeVector (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This funciton computes the ivnerse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cachesolve should get the inverse from the cache.
## if the inverse has not been claculated, then the matrix gets stored in data, 
## and m calculates the inverse and x$setinverse(m) stores it in the object m in makeCacheMatrix

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
        ## Return a matrix that is the inverse of 'x'
}
