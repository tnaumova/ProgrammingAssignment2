## Creates wrapper to matrix object, which allows to cache calculated inverse matrix

## Returns object which allows to cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) { 
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverseMatrix <<- inv 
  getInverse <- function() inverseMatrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculates inverse matrix or returnes previously calculate

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(is.null(inv)) {
    message("Calculating inverse matrix")
    data <- x$get()
    inv <- chol2inv(data)
    x$setInverse(inv)
  } 
  inv
}
