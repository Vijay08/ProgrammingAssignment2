#The assignment is about cache the inverse of matix
#Rather than computing the inverse of the matrix its better to cache it avoid repeated computation

#The below function creates a special object matrix to cache the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_mat <<- inverse
  getInverse <- function() inv_mat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#The below function computes the matrix inverse of the special  matrix from the above function. 
#The inverse of the matrix is cached if its not the same matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getInverse()
  if (!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  mat <- x$get()
  inv_mat <- solve(mat, ...)
  x$setInverse(inv_mat)
  inv_mat
}
