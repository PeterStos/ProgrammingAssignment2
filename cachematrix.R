## The functions create a special object that stores a matrix and cache's its 
## inverse.

## This Function creates a list with functions to set a metrix, get the matrix, 
## setInvMatrix (set the inverse of a matrix) and getInvMatrix (get the inverse) 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) m <<- solve
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)

}

## Function calculates the inverse of a matrix created with the above function

cacheSolve <- function(x=0) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) { # test if m is not null
    message("getting cached data")
    return(m)
  }
  data <- x$get() # saves the vector to data
  m <- solve(data) # calculates the mean of the vector (only here)
  x$setInvMatrix(m) # set the mean in the cache
  m
}


# TESTS
# mt <- matrix(data = c(1,2,-3,4,5,-6,7,8,9),nrow = 3, ncol = 3)
# test <- makeCacheMatrix(mt)
# cacheSolve(test)


