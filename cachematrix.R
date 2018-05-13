## Put comments here that give an overall description of what your
## functions do

## This function creates a "matrix" that is a list which sets the value of the matrix,
## gets the value of the matrix, sets the inverse of the matrix, and gets the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setIinverse <- function(inverse) inv <<- inverse
  getInverse <- function() inverse
  list(set = set, get = get, setIinverse = setInverse, getInverse = getInverse)
}


## This function checks to see if the inverse of a matrix is already cached. If it is,
##, the function displays the cached value. Otherwise it calculates the inverse of the 
## matrix and displays that value

cacheSolve <- function(x, ...) {  
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
