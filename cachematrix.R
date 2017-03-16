## The following two functions create an object to hold a matrix, it's inverse matrix and functions
## to get and set each of both. The second function gives back the inverse matrix to a given matrix. 
## The matrix is given as an object as created by the first function.

## This function stores a matrix and it's inverse matrix. It is called with a matrix. This matrix is kept and
## can be fetched from outside by using the get function of makeCacheMatrix. It allows to set the matrix by the 
## set function. When setting the matrix, the inverse matrix is set to null. Corresponding functions exists 
## to set and get the inverse matrix. 
makeCacheMatrix <- function(x = matrix()) {
  ## declare the inverse matrix and set its value to null
  i <- NULL  ## the inverse matrix
  
  ## the matrix is stored and the inverse matrix is initialized
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## gives back the matrix
  get <- function() {
    x
  }
  
  ## sets the inverse matrix
  setInvMat <- function(inverse) {
    i <<- inverse
  }
  
  ## gives back the inverse matrix
  getInvMat <- function() {
    i
  }
  
  ## returns a listobject referencing the previously declared functions
  list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}

## This function receives an object of type makeCacheMatrix. cacheSolve first tries to retrieve the
## inverse matrix from this object. If successful, this inverse matrix is returned. If not successful,
## the function calculates the inverse matrix using the solve function and afterwards stores this 
## inverse matrix in the makeCacheMatrix object by calling its setInvMat function with the previously
## calculated inverse matrix.
cacheSolve <- function(x, ...) {
  
  ## try to fetch the inverse matrix from the cache
  i <- x$getInvMat()
  if (!is.null(i)) {
    ## the invere matrix has been found
    return(i)
  } 
  ## the inverse matrix is not cached, therefore
  ## get the matrix
  m <- x$get()
  ## calculate the inverse matrix
  i <- solve(m, ...)
  ## cache the inverse matrix in the makeCacheMatrix object x
  x$setInvMat(i)
  ## return the inverse matrix
  i
}
