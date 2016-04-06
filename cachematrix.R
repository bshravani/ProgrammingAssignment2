
## makeCacheMatrix function creates the matrix for the given values and dimension and returns the inverse of the given matrix with below member functions
## set() function creates the matrix
## get() function Returns the matrix created
## setinverse() function performance matrix inversion
## getinverse() function Returns matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve() function Computes, caches, and returns matrix inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Trail run
## yy = makeCacheMatrix(matrix(c(45,24,11,41), nrow=2, ncol=2))
## yy$get()         
## cacheSolve(yy)   
## yy$getinverse()  
## cacheSolve(yy)   
