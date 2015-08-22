## These functions demonstrate how lexical scoping can be used within R to cache 
## results of functions to avoid repeating slow and complex computations if a 
## result is already cached

## The makeCacheMatrix function creates a special object that is able to cache it's inverse
## The special matrix is a list which sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse and gets the value of the inverse

## Usage: First create a matrix - example: x <- matrix(rnorm(9),nrow=3)
## Usage: Then run the function: mx <- makeCacheMatrix(x)
## Usage: Then get the result: mx$get()
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcmat <- function(inver) m <<- inver
  getcmat <- function() m
  list(set = set, get = get,
       setcmat = setcmat,
       getcmat = getcmat)
  
}

## The cacheSolve function calculates the inverse of the special matrix using 
## the solve function. If the result is already cached then the result is retrieved from the cache

## Usage: First run to get an uncached result - Example: cacheSolve(mx)
## Usage: Second run to get the cached result: cacheSolve(mx)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getcmat()
    if(!is.null(m)) {
      message("Getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setcmat(m)
    m
  
}
