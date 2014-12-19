


makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  ## It gets a matix as input, and creates an object containing 
  ## the original matrix and the cached inverse (initially null)
  
  inv <- NULL # cached inverse starts out as null
  
  set <- function(y){
  ## set the initial values  
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x #return the original matrix
  
  setinverse <-function(solve) inv <<- solve #method to set the value of the cache to the inverse
  getinverse <- function() inv #method to return the cached value
  
  #return a list
  list (set = set, get= get, setinverse = setinverse, getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
  ## should retrieve the inverse from the cache.
  
  ## get the cached value so far (will be null if inverse hasn't been calculated yet)
  inv <- x$getinverse()
  
  ##if there's a cached value, use it
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  
  #otherwise, compute it
  data <- x$get() #get the original matrix
  inv <- (solve(data, ...)) #compute the inverse
      x$setinverse(inv) #put the inverse into the cache
      inv #return the inverse we just computed
}
