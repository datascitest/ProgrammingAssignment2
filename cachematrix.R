## Programming Assignment 2: Lexical Scoping 
## Assignment: Caching the Inverse of a Matrix


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## init set inverse as NULL
  inverse <- NULL
  ## set function: set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## get function: get the value of the matrix
  get <- function() x
  ## setinverse function to set the inverse of the matrix
  ## assign the inverse in the environment with <<-
  setinverse <- function(inversion) inverse <<- inversion
  ## getinverse function to get the inverse of the matrix
  getinverse <- function() inverse
  ## return value as a list of all the functions above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## x is the return of the function above as a list.
cacheSolve <- function(x, ...) {
  ## get inverse from getinverse function.
  inverse <- x$getinverse()
  ## if inverse exists in cache, then return it. 
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  ## else, get the matrix
  data <- x$get()
  ## compute the inverse of the matrix with solve()
  inverse <- solve(data, ...)
  ## set the inverse in cache.
  x$setinverse(inverse)
  ## Return a matrix that is the inverse of 'x'
  inverse
}

## simple unit test
## m as a invertible matrix
m <- matrix(1:4, nrow=2)


M2C <- makeCacheMatrix(m)

## get the matrix
M2C$get()
## get the inverse of the matrix first time.
cacheSolve(M2C)
## get the inverse of the matrix second time. (from cache)
cacheSolve(M2C)
## also you can assign the value from cache to variable:solved
solved <- cacheSolve(M2C)
## then test if A*A^(-1) = I
M2C$get()%*%solved
