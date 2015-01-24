## Function makeCacheMatrix() creates a special "matrix", which is really a list containing
## a function to 
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve() inverts the special "matrix" created with function makeCacheMatrix()
## This function first checks to see if the inverse has already been created. If so, then its
## gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## of the matrix using function solve() and sets the value of the inverse in the cache
## via the setinverse function.
## Note that it is assumed that the input matrix is invertible (i.e. it is a square matrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("Getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
