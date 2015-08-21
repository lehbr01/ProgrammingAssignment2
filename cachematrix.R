
##THis function will change from numeric vector to matrix vector

makeCacheMatrix <- function(x = matrix()) {
  
  m = NULL 
  set = function(y) {  ##sets the matrix vector from whatever is passed in
    x <<- y
    m <<- NULL
  }
  get = function() x  ## gets the matrix vecor
  setinv = function(inverse) m <<- inverse   ##function that sets inverse of matrix
  getinv = function() m ##function to get the inverse matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv) ##returns a list of the functions
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  m = x$getinv()
  
  if (!is.null(m)) #checks to see if matrix has a value cached
  {
    message("getting cached data") 
    return(m) ##returns cached matrix.
  }
  
  matrix = x$get()
  m = solve(matrix, ...)
  
  x$setinv(m)
  
  return(m)
}
