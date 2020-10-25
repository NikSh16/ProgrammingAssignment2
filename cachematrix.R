## This function creates a 'matrix' object that can cache its 
## inverse. sample is the matrix object that user will submit on 
## the console

makeCacheMatrix <- function(x = matrix()) {
  imsample <- NULL
  set <- function(x){
    sample <<- x
    imsample <<- NULL
  }
  get <- function() sample
  setInverse <- function(inverse) im <<- inverse
  getInverse <- function() imsample
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## function to compute inverse of the matrix created by 
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- sample$getInverse()
  if (!is.null(im)){
    message("got cached data")
    return(imsample)
  }
  mat <- sample$get()
  imsample <- solve(mat, ...)
  sample$setInverse(imsample)
  imsample
}
