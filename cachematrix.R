## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## sets the matrix value and the inverse value 
## gets the values of the matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set=set, get=get,setinverse=setinverse, getinverse= getinverse)
}


## Write a short comment describing this function
## checks cache for inverse of given matrix 
## if not there then will calculate the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
