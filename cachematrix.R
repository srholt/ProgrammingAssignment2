makeCacheMatrix <- function(x = matrix()) {
  ## This function returns a list that contains the following functions:
  ##  set a matrix, get a matrix
  ##  set an inverse, get an inverse
  inv = NULL
  set = function(y) {
    # uses `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## uses output of makeCacheMatrix()
  ## and returns the inverse of the matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  # From here a loop is used: if the inverse is already calculated get from cache and don't calculate
  ## otherwise go ahead and calculate the inverse

  if (!is.null(inv)){
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # final part sets the value of the inverse in the cache
  x$setinv(inv)
  return(inv)
}