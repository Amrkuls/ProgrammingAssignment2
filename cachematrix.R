## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initializing the matrix to NULL value
  invmat  <- NULL
  set <- function(y) 
  {
    x <<- y
    invmat <<- NULL
  }
  get <- function() 
  {
    x
  }
  
  setinv <- function(inv)
  { 
    invmat <<- inv
  }
  getinv <- function()
  {
    invmat
  } 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinv()
  
  if(!is.null(invmat)) 
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}

callfunctions <- function() 
{
  m <- makeCacheMatrix(matrix(1:9, 3, 3))
  m$get()
  m$getinv()
  cacheSolve(m)
  m$set(matrix(c(1,12,28,10,24,33,45,60,21), nrow=3, ncol=3)) # Modify existing matrix
  cacheSolve(m)   # Computes, caches, and returns new matrix inverse
  m$get()         # Returns matrix
  m$getinv()  # Returns matrix inverse    
  m$get()%*% m$getinv()  # returns the identity matrix
  
}


