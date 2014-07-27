## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix())
{
## This function create a list with:
## 1-) A function, named 'setMatrix', which stores the matrix value
## 2-) A function, named 'getMatrix', which returns the matrix value
## 3-) A function, named 'setInverseMatrix', which stores the inverse matrix value
## 4-) A function, named 'getInverseMatrix', which returns the matrix value

  inverse <- NULL

  setMatrix <- function(matrix)
  {
    x <<- matrix
    inverse <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(inverseMatrix) inverse <<- inverseMatrix
  getInverseMatrix <- function() inverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

cacheSolve <- function(x, ...)
{
## This function returns the inverse of the matrix 'x'
## Before making calculations, it will search for previous matrix which had their
## inverse matrix calculated before
## In case the given matrix didn't have the inverse matrix stored
## then the 'solve' function will be called

inverse <- x$getInverseMatrix()

if(!is.null(inverse))
{
  message("Getting Cached Matrix")
  return(inverse)
}

newMatrix <- x$getMatrix()
inverse <- solve(newMatrix, ...)
x$setInverseMatrix(inverse)
inverse
}