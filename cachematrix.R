makeCacheMatrix <- function(x = matrix()) {
  ## This function returns a list that contains the following functions:
  ##  set a matrix, get a matrix
  ##  set an inverse, get an inverse
  i = NULL
  set = function(y) {
    # uses `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinv = function(inverse) i <<- inverse 
  getinv = function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## This function uses output of makeCacheMatrix()
  ## and returns the inverse of the matrix input to makeCacheMatrix()
  
  i = x$getinv()
  # From here a loop is used: if the inverse is already calculated get from cache and don't calculate
  ## otherwise go ahead and calculate the inverse

  if (!is.null(i)){
    return(i)
  }
  matrixdata = x$get()
  i = solve(matrixdata, ...)
  
  # final part sets the value of the inverse in the cache
  x$setinv(i)
  return(i)
}