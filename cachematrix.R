## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create cache for Inverse matrix, including set, get, setInv, getInt
## use getInv to get the cached value. 

makeCacheMatrix <- function(x = matrix()) {
  InvX <- NULL
  set <- function (y) {
    x <<- y
    InvX <<- NULL
  }
  get <- function() x
  setInv <- function(inv_x) InvX <<- inv_x
  getInv <- function() InvX
  list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## Return cached inverse of 'x' if alreay there. 
## Otherwise, calculate inverse of 'x' with solve, cache inverse and return it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getInv()
  if (!is.null(inv_x)){
    message("getting cached inversed matrix")
    return (inv_x)
  }
  
  set_matrix <- x$get()
  inv_x <- solve(set_matrix, ...)
  x$setInv(inv_x)
  inv_x
}
