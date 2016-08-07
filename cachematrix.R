## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix())  {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function
## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available
#The following cacheSolve function calculates the inverse matrix of the special "matrix"

#created with the above makeCacheMatrix function.

#It will first check to see if the inverse has already been calculated. If so, it gets

#the inverse from the cache and skips the computation. Otherwise, it calculates the inverse

#of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
