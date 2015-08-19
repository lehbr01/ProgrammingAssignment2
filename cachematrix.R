## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) ##change from numeric to matrix vector
{
  
  m = NULL 
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setinv = function(inverse) m <<- inverse   ##function that sets inverse of matrix
  getinv = function() m ##function to get the inverse matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
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
