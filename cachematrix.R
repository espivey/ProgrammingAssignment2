## The function 'makeCacheMatrix' creates and retains the inverse of 
##   a square matrix.  The function 'cacheSolve' checks to see if the 
##   inverse exist and is developed from the desired matrix. 

##   Always initiallize makeCacheMatrix() before executing function 

##  An example of use is as follows:

#  a <- c(16,2,3,13,5,11,10,8,9,7,6,12,4,14,15,12)
#  A = matrix(a,4,4, byrow = TRUE)
#  m <- makeCacheMatrix()

#  m$setMatrix(A)

#  m$getMatrix()

#  m$getInverse()

#  iX <- cacheSolve(m, A);

##  'makeCacheMatrix()' must be executed before 'cacheSolve(m, A)'. 
##  If 'cacheSolve(m, A)' is initially executed without first initializing 
##  'makeCacheMatrix()', an error will occur.

## Upon executing 'cacheSolve(m, A)', if the matrix has not been set or 
##  the Matrix 'A' referenced in  'cacheSolve(m, A)' is not the matrix
##  of the inverted matrix, the Matrix A will be inverted and returned,
##  else the stored inverted matrix generated within 'makeCacheMatrix()' 
##  will be returned.


## The function 'makeCacheMatrix' creates and retains the inverse of a square 
##   matrix using the functions 'setMatrix', 'getMatrix', 'getInverse'.
##   'x' is the object of the stored matrix and its invert.

makeCacheMatrix <- function(x = matrix()) {
     
     iX <- NULL
     setMatrix <- function(Y) {
          x <<- Y
          iX <<- solve(A)
      }

     getMatrix <- function() x
     getInverse <- function() iX
     
     list(setMatrix = setMatrix,
          getMatrix = getMatrix,
          getInverse = getInverse)
}


## The function 'cacheSolve' returns the invert of the Matrix A
##   stored in object x. 

cacheSolve <- function(x, A) {
        ## Return a matrix that is the inverse of 'X'

     oX <- x$getMatrix()
     iX <- x$getInverse()
     if(!is.null(iX) & identical(oX, A)) {
          message("getting cached inverted Matrix")
          return(iX)
     }

     x$setMatrix(A)
     x$getInverse()
}




## Example 

a <- c(16,2,3,13,5,11,10,8,9,7,6,12,4,14,15,12)
A = matrix(a,4,4, byrow = TRUE)
m <- makeCacheMatrix()

m$setMatrix(A)

m$getMatrix()

m$getInverse()

iX <- cacheSolve(m, A); iX














