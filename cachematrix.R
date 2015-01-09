## Cache Matrix Inverse.

## Special Wrapper Object for a Matrix
## Has the Matrix and its inverse as the properties
## Has getters and setters
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInv <- function (inverse) {
    inv <<- inverse
  } 
  getInv <- function () {
    inv
  }
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Computes the inverse of a matrix
## If the inverse is not present, it computes the inverse, saves it and returns the computed value
## If the inverse for a matrix is already present, it returns the saved value instead.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(is.null(inv)) {
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
  } else {
    message("getting cached inverse")    
  }
  inv
}
