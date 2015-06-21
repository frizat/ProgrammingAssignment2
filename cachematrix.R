## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object that can cache its inverse
## It has setter and getter for new or cached value as a holder to/from memory
makeCacheMatrix <- function(x = matrix()) {
 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve){
    m <<- solve
  } 
  
  getsolve <- function() {
    m
  }
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  
  ## Return current value of inverse matrix 'x' assign to m
  m <- x$getsolve()
  
  ## If m has the same value before, then system returns message
  if(!is.null(m)) {
    message("Getting cached data!")
    return(m)
  }
  ##Else get the new matrix 
  data <- x$get()
  ## Inverse the matrix
  m<-solve(data) %*% data
  ##Set to current memory
  x$setsolve(m)
  ##Returns new m value
  m
}


##Test output
a <- makeCacheMatrix(matrix(c(3,1,2,4),nrow=2,ncol=2))
cacheSolve (a) ## First attempt
cacheSolve (a) ## Second attempt

b<- makeCacheMatrix(matrix(c(1,2,-3,4,5,-6,7,8,9),nrow=3,ncol=3))
cacheSolve (b) ## First attempt
cacheSolve (b) ## Second attempt
