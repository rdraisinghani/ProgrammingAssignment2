##The below functions help optimizing time consuming operation of finding an inverse
## of a matrix by caching the inverse of the matrix

## The function takes a matrix as a parameter and returns a wrapper object which exposes four
## functions namely get,set,getInverse and setInverse.
## Use the get and set to return and assign the orignial matrix respectively
## use the getInverse and setInverse to return and assign the inverse of
##orignial matrix respectively.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat){
    x <<- mat
    inv<<- NULL
  }
  
  get <- function() x
  
  getInverse <- function() inv
  
  setInverse <- function(inverse) inv <<- inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve expects an object of type makeCacheMatrix.
## It returns the inverse of the matrix assigned to x
## If x.m is not null it returns the cached inverse of the matrix
## otherwise it solves the matrix and assings it to m 
##and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached inverse of matrix")
    return(inv)
  }
  
  orignalMatrix <- x$get()
  inv <- solve(orignalMatrix)
  x$setInverse(inv)
  inv
}
