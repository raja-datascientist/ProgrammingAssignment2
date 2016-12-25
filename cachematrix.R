## creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## x is a square invertible matrix
  ## return - a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## computes the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## it’ll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {

  ## x is a output of makeCacheMatrix()
  ## return - inverse of the original matrix input to makeCacheMatrix()
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat1 <- x$get()
  inv <- solve(mat1, ...)
  x$setinv(inv)
  return(inv)
}
