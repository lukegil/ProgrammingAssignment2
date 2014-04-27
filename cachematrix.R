## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##calculates and caches matrices and their inverses

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #this is our matrix!
  matrixVal <- function() x
  #reset the matrix's value
  resetMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  #create inverse function
  doInvertMatrix <- function(x) {
    invMatrix <- solve(x)
  }  
  
  #return inverse function
  getInvertMatrix <- function() invMatrix
  
  #set cache value--namely, pass cache back into  makeCacheMatrix's scope
  cacheMatrix <- function(x) invMatrix <<- x
  
  functionList <- list(getInvertMatrix = getInvertMatrix, doInvertMatrix = doInvertMatrix, cacheMatrix = cacheMatrix, matrixVal = matrixVal, resetMatrix = resetMatrix)
  
}


## Write a short comment describing this function
##will calculate an inverse matrix if necessary or just use a cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #if some value for this already exists, use that  
  invMatrix <- x$getInvertMatrix()
  
  if (!is.null(invMatrix)) {
      print("getting cache data")
      return(invMatrix)
    }

  #Otherwise, find the inverse of the given matrix  
  invMatrix <-  solve(x$matrixVal())
  
  #save it for later
  x$cacheMatrix(invMatrix)
  
  #return the inverse
  invMatrix
  
}
