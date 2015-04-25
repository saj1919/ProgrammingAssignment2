### This code is in response to Programming assignment 2 for 
### the course 'R Programming' (https://class.coursera.org/rprog-013)


######################################## START ############################################

### This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ### Set inverse matrix to be computed to NULL initially
  i_matrix <- NULL
  
  ### Set new Matrix Variable 
  set_Matrix <- function(new_x) {
    x <<- new_x
    
    ### Set inverse matrix to NULL for new value of x
    i_matrix <<- NULL
  }
  
  ### Get value of Matrix x
  get_Matrix <- function() {
    x
  }
  
  ### Set/Cache Inverse function 'solve' return value in i_matrix
  set_Inverse <- function(solve) {
    i_matrix <<- solve
  }
  
  ### Get the cached value of inverse matrix
  get_Inverse <- function() {
    i_matrix
  }
  
  ### Function returns list of functions to operate
  list(set_Matrix = set_Matrix, get_Matrix = get_Matrix, set_Inverse = set_Inverse, get_Inverse = get_Inverse)

}


### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
### If the inverse has already been calculated (and the matrix has not changed), then the 
### cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

  ### Try to read from cache first
  i_matrix <- x$get_Inverse()
  
  ### Return i_matrix if not null
  if(!is.null(i_matrix)) {
    message("Getting Cached Inverse-Matrix")
    return(i_matrix)
  }
  
  ### Else calculate new value for inverse matrix and cache it
  matrix <- x$get_Matrix()
  i_matrix <- solve(matrix)
  x$set_Inverse(i_matrix)
  
  ### Return inverse matrix
  i_matrix
}

###################################### EVALUATION ############################################

### Example : Create Matrix B
B = matrix(c(2, 4, 3, 1, 5, 7, 8, 6, 9), nrow=3, ncol=3)

### Print B
B

### Get inverse of B by normal way
solve(B)

### Create new cache matrix of B
newB <- makeCacheMatrix(B)

### Print cache matrix
newB

### Get Inverse of B from cache matrix
cacheSolve(newB)

########################################## END #################################################
