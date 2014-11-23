## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## #############################################################################
## Function creates a matrix object with cachable inverse
##
##  x                            formal parameter (source matrix)
##
##  inv                          variable (inverse matrix)
##
##  set and get                  methods that allow the stored matrix
##                               to be saved or retrieved
##  set.Inverse and get.Inverse  methods that allow the stored inverse 
##                               of the matrix  to be saved or retrieved
## =============================================================================

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse
  inv <- NULL                       
  
  ## Method to save the stored marix
  set <- function(matrix) {            
    x <<- matrix
    inv <<- NULL
  }
  
  ## method to retrieve the stored marix
  get <- function() x                     
  
  ## Method to save the stored inverse of the matrix 
  set.Inverse <- function(inverse) inv <<- inverse  
  
  ## Method to retrieve the stored inverse of the matrix
  get.Inverse <- function() inv                     
  
  ## Return a list of the methods
  list(set = set, get = get,
       set.Inverse = set.Inverse,
       get.Inverse = get.Inverse
  )
  
}


## Write a short comment describing this function

## #############################################################################
## Function calculates and returns inverse of matrix object, 
##       or returns inverse immediately if it has already been calculated
##
##  x                            formal parameter (inverse matrix)
##
## =============================================================================

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse from the cachable matrix
  inv <- x$get.Inverse()
  
  ## send a message if it was already calculated, ... and return 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## else - calculate the inverse, 
  ##        store it in the cachable matrix
  ##        and return the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$set.Inverse(inv)
  inv				
}


## #########################################################
##                                                        ##
## testing                                                ##
##                                                        ##
## =========================================================

## prepare test data ---------------------------------------
matrix1 <- matrix(c(1, 2, 2, 2, 2, 2, 2, 2, 1), ncol = 3)      # example 3x3 matrix
matrix2 <- matrix(c(-1, 1, 0, 1, -1.5, 1, 0, 1, -1), ncol = 3) # calculated inverse of matrix1

## test cachable inverse -----------------------------------
x <- makeCacheMatrix(matrix1)

matrix3 <- cacheSolve(x)     # no  message - must calculate the inverse variable
matrix4 <- cacheSolve(x)     # message "getting cached data"

identical(matrix2, matrix3) && identical(matrix2, matrix4)  


