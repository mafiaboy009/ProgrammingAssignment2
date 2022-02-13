## makeCacheMatrix takes a matrix and returns a list
## 
## Consider this list as a special matrix and the list has
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #matInverse <- matrix() 
  matInverse <- NULL
  
  set <- function(y) {
    x <<- y        
    matInverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) matInverse <<- inverse
  
  getinverse <- function() matInverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The following function calculates the inverse of the special "matrix" created
## with the above function.

cacheSolve <- function(x, ...) {
  
  matInverse <- x$getinverse()
  
  if(!is.null(matInverse)) {
    message("getting cached data")
    return(matInverse)
  }
  
  data <- x$get()
  
  matInverse <- solve(data, ...)
  
  x$setinverse(matInverse)
  
  matInverse
}
