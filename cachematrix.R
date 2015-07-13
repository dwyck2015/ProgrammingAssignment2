## This pair of functions cache the inverse of a matrix.
## Matrix inversion is often a costly computation and there can be a benefit to caching
## the inverse  of a matrix rather than computing it repeatedly.
## These function assumed that the supplied matrix is always invertible.

## Example to run:
# m <- matrix(c(1,3,5,6,2,4,7,9,8),nrow=3,ncol=3) # create invertible 3x3 matrix
# listmat <- makeCacheMatrix(m) #create special list matrix
# cacheSolve(listmat) # will return calculated inverse of matrix m
# cacheSolve(listmat) # will return cached inverse of matrix m

## The function makeCacheMatrix creates a special "matrix": a list containing functions to:
## 1) set the inverse of the matrix
## 2) get the inverse of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has alraedy been calculated (and the matrix has
## not changed), then the cacheSole function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
