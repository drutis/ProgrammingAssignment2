## This function Function makeCacheMatrix gets a matrix as an input, sets the value of the matrix,
#gets the value of the matrix, sets the inverse Matrix and gets the inverse Matrix. The matrix object
#can cache its own object. 

#<<- operator is used to assign a value to an object in an environment that is different 
#from the current environment 

#take the matrix as an input

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x #get the value of matrix
  setInverse <- function(inverse) invMatrix <<- inverse #set value of inverse matrix
  getInverse <- function() invMatrix #get value of inverse matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse= getInverse)
}


##  The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and sets the inverse of  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached inverse matrix" 
#and the cached object

cacheSolve <- function(x, ...) {
  #get value of inverse matrix from makeCacheMatrix function
  invMatrix <- x$getInverse() 
  if(!is.null(invMatrix)) { #if inverse matrix is not null
    message("getting cached inverse matrix") #type message
    return(invMatrix) #return inverse matrix
  }
  data <- x$getMatrix() #get original matrix data
  invMatrix <- solve(data, ...) #use solve function to find inverse of matrix
  x$setInverse(invMatrix) #set inverse of matrix
  invMatrix #return inverse of matrix
}
