## Comment on this assignment
## The two functions below work the same way as the makeVector and cacheMean functions do in the example code. However, 
## there's an additional requirement, in parenthesis, about checking to see if the input matrix has changed or not.

## Unfortunately, these two functions are not well designed for checking to see if the input matrix has changed or not.
## Note that makeCacheMatrix actually caches the input matrix, so it essentially creates an object that contains both a 
## a matrix and its inverse. However, cacheSolve is expecting a cacheMatrix object, not a matrix, as its 'z' parameter.
## I could write a set of functions that clean this up, so that cacheSolve actually takes a matrix rather than a cacheMatrix,
## but frankly it wasn't worth it.

## The makeCacheMatrix function takes a matrix, caches it, and can cache the inverse of the matrix. Note that 
## makeCacheMatrix does NOT calculate the inverse of the matrix (that is done by the cacheSolve function) but can 
## cache the inverse of the stored matrix after it is computed by cacheSolve. This function essentially creates an
## object of type 'cacheMatrix'.

## Sample code for using makeCacheMatrix is as follows:
## my_matrix <- matrix(1:4, 2, 2)
## cacheM <- makeCacheMatrix(my_matrix)
## This has been successfully tested with the version of cacheSolve below.

makeCacheMatrix <- function(x = matrix()) {
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




## The cacheSolve function takes a cacheMatrix as input, and returns the inverse of the matrix stored in the 'x' field
## of the cacheMatrix. If the inverse is not cached, cacheSolve computes the inverse of 'x' and stores it in the 'inv' 
## field of the cacheMatrix. Assuming that cacheM is a cacheMatrix object as defined above, this function is called as:
## cacheSolve(cacheM), and has been tested as correct.


cacheSolve <- function(z, ...) {
   
  
   inv <- z$getinv()
   if(!is.null(inv)) {
     message("getting cached data")
     return(inv)
   }
   data <- z$get()
   inv <- solve(data, ...)
   z$setinv(inv)
   inv
  
}
