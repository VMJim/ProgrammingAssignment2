## makeCacheMatrix(...) models a "matrix object" of which the inverse is cached.
## This may be useful given that matrix inversion can be a costly procedure. 
## If the inverse has been calculated previously, it is not nescessary to do so 
## again: it can then be simply looked up in memory. 
## The function cacheSolve(...) provides this feature for the "matrix object" 
## mentioned above.

## The function makeCacheMatrix(...) models a special "matrix object" that can 
## cache its inverse. 
## INPUT: a matrix
## OUPUT: a list of methods for manipulating the object.
makeCacheMatrix <- function(X = matrix()) {
      
      cached_inv_X <- NULL 
      
      # SETTER methods: 
      
      # set(...) allows a caller to set the matrix.
      set <- function(Y) {
            X <<- Y
            cached_inv_X <<- NULL
      }
      
      # setInverse(...) allows a caller to set the inverse.
      setInverse <- function(invX) {
            cached_inv_X <<- invX
      }
      
      # GETTER methods:
      
      # get(...) is a getter method: returns the matrix itself. 
      get <- function() {
            X
      }
      # getInverse(...) is a getter method: it returns the inverse of the matrix.
      getInverse <- function() {
            cached_inv_X
      }
      
      # RETURN argument:
      list(set = set, 
           setInverse = setInverse,
           get = get, 
           getInverse = getInverse)
}


## cacheSolve(...) computes the inverse of the "matrix object" modelled by 
## makeCacheMatrix(...) above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve(...) retrieves 
## the inverse from the cache.
## INPUT: a CacheMatrix object
## OUTPUT: its inverse
cacheSolve <- function(X,...){

      inv_X <- X$getInverse()
      
      if(is.null(inv_X)) # if cache is empty, then:
      {
            inv_X <- solve(X$get())    
            X$setInverse(inv_X)
            inv_X # returns the inverse
      }
      else # if cache is not empty, then:
      {
            inv_X # returns the inverse
      }
      
}
