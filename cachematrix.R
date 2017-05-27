## makeCacheMatrix function takes a matrix (enter a square matrix) as an argument
## and it creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x= matrix()){
  inverseMatrix <- NULL
  setMatrix <- function(y){
    x <<- y
  }
  getMatrix <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(setMatrix=setMatrix, getMatrix=getMatrix, setinverse= setinverse, getinverse=getinverse)
}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## if the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache
## it throws an error if "matrix" is non-invertable, i.e. det(matrix) = 0

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$getMatrix()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
