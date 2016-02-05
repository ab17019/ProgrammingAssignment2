## Create a caching mechanism for calculating the Inverse of a Matrix
## First 2 functions work together to get the inverse of a Matrix 
## The third function is a test for the funtionality

## This function creates a special "matrix" object that can cache its inverse
## The new object will have functions to set/get the matrix/inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function (y){
    x <<- y
    invMatrix <<- NULL
  }
  get <- function () x
  setInv <- function(inv) invMatrix <<- inv
  getInv <- function () invMatrix
  list( set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInv()
  if (!is.null(invMatrix)){
    message("Return from cache")
    return (invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, diag(nrow(data)), ...)
  x$setInv (invMatrix)
  ## Return a matrix that is the inverse of 'x'
  invMatrix
}

## 
## Test the above 2 functions by retriving the inverse if there is one twice X * Inv(X)
## Example of run: testSolve(rbind(c(1, -1/4), c(-1/4, 1)) )
testSolve <- function(x = matrix()){
  mat = makeCacheMatrix(x)
  if ( identical(cacheSolve(mat) %*% mat$get(), diag(nrow(x)))) {
    message("We verified thet X*Inv(X) = I now return the Inv(x)")
    return(cacheSolve(mat))
  } else
    message("NonInversable Matrix")
  
}