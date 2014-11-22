## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## #############################################################################
## Creates a matrix object with cachable inverse
##
##  x                            formal parameter (source matrix)
##
##  inv                          variable in which the inverse will be saved
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

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}