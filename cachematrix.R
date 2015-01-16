## Below are two functions used for matrix inversion. Instead of forcing
## the computer to calculate the inverse of a matrix every time we need it
## (which can be quite taxing on large matrices) the result is cached the
## first time it is calculated. Upon further calls for the inverse of the matrix,
## the cached result is called. The information of the matrix and its inverse (as 
## well as getter and setter methods for both) is stored in a list of objects.

## This is a function that can stores information about a matrix.
## It saves the matrix, can set a new matrix, save the inverse of the matrix,
## and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL #inverse matrix
  get <- function() { x }
  set <- function(y = matrix()) {
    x <<- y
    im <<- NULL
  }
  getInverse <- function() { im }
  setInverse <- function(inverse) {
    im <<- inverse
  }
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function gets the inverse of a matrix. If the inverse is not stored
## it will calculate it and store the result. If the inverse is already stoted 
## it will simply print the stored result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Getting inverse from cache")
    return(i)
  }
  else{
    message("Calculating inverse for the first time...")
    message("Caching inverse")
    i <- x$setInverse(solve(x$get()))
  }
}
