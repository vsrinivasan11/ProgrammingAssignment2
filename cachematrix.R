## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function that creates a matrix 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y                    ## set new matrix
    m <<- NULL
  }
  
  get <- function() x          ## Returns the original matrix
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m   ## Returns matrix inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix is the same as thr original), 
## then cachesolve will return the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  ## the inverse of the matrix is actually done here
  x$setinverse(m)
  m
}
  