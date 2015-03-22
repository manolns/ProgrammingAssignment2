### This script  creates a special "matrix" object that can cache its inverse
## and computes the inverse of the special "matrix" returned by makeCacheMatrix above.

# makeCacheMatrix is a function that creates a list of functions when you call it.
# set: set the value of the matrix 
# get: get the value of the matrix
# setinv: set the value of inverse matrix, using the solve function
# getinv: get the value of inverse matrix
# finally, return a list with all of this functions

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
  
}


# cacheSolve returns the inverse of the matrix. It checks if
# the inverse matrix has already been entered. If so, it returns the result of inverse matrix and skips the
# function, besides it displays a message saying that "getting cached data". 
# If not, it calculates the inverse of the matrix and sets the value in the 
# setsolve function.

# Besides, This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

## Test
#B = matrix(rnorm(100,mean = 0,sd = 1),nrow=10,ncol=10)
#a <- makeCacheMatrix(B)
#c <- cacheSolve(a)
#round(c %*% B)
