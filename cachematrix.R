## Put comments here that give an overall description of what your
## functions do
#
#function makeCacheMatrix() creates a matrix which is able to cache its inverse
#         it receives a matrix as parameter
#function cacheSolve() computes the inverse of the "matrix" returned by makeCacheMatrix
#          i.e. it receives as parameter the matrix returned by function makeCacheMatrix()
# functions work only with squared matrices or those having defined inverse matrix


##---------------------------------------------------
## This function creates a "special matrix" from an input matrix
## If already available, its inverse can be set using function setinver()
## To recover the inverse matrix from environment, function getinver() can be used
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
   
  #setting input parameter "y" as values to special matrix "x"
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  # this function returns the value of matrix x
  get <- function() x
  
  # this function sets input paramenter inv as the value inver (inverse)
  setinver <- function(inv) inver <<- inv
  
  # this function returns the value inver (inverse)
  getinver <- function() inver
  
  # list of functions returned by function makeCacheMatrix
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)  
}


##------------------------------
## This function computes the inverse of the special matrix returned from 
## function makeCacheMatrix
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # retrieving value of the inverse   
  inver <- x$getinver()
  
  # if inver is not null, it retrieves it from cached matrix
  if(!is.null(inver)) {
    message("getting cached matrix")
    return(inver)
  }
  
  # retrives the value of matrix x
  data <- x$get()
  
  # computes the inverse of a squared matrix
  inver <- solve(data)

  #sets the just computed inverse (inver) of x
  x$setinver(inver)
  
  #returned value of function cacheSolve()
  inver
}
