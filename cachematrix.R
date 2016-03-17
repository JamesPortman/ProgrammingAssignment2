## Data Science Specialization Track
## R Programming Course Assignment 2
## By: James Portman - March 17, 2016
##
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
##
## This assignment contains two functions
## 1) makeCacheMatrix: This function creates a special object that can cache its inverse.
## 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed),  
##    then the cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special object, that is really a list, containing a function to:
  # 1. Set the matrix.
  # 2. Get the matrix.
  # 3. Set the inverse of the matrix.
  # 4. Get the inverse of the matrix.
  # 5. Create the list.
  
  inv <- NULL ## Initialize to NULL
  
  # 1. Set the matrix.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 2. Get the matrix.
  get <- function() x
  
  # 3. Set the inverse of the matrix.
  setInverse <- function(inverse) inv <<- inverse

  # 4. Get the inverse of the matrix.
  getInverse <- function() inv
  
  # 5. Create the list.
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  # This function calculates the inverse of a matrix via the following steps:
  # 1. Get the current state of the inverse of the matrix.
  # 2. If already calculated, it gets the inverse from the cache and skips the computation.
  # 3. If not already calculated then:
  #    3a. Calculate the inverse of the matrix.
  #    3b. Set the value of the inverse in the cache via the makeCacheMatrix function
  
  # 1. Get the current state of the inverse of the matrix.
  inv <- x$getInverse()
  
  # 2. If already calculated, it gets the inverse from the cache and skips the computation.
  if(!is.null(inv)) {
    message("Returning cached matrix")
    return(inv)
  }
  
  # 3. Inverse not already calculated.
  
  # 3a. Calculate the inverse of the matrix. 
  data <- x$get()
  inv <- solve(data, ...)
  
  # 3b. Set the value of the inverse in the cache via the makeCacheMatrix function.
  x$setInverse(inv)
  inv ## Last line is the return value.
}


## TESTING
# Create a matrix that is invertible. 
x <- matrix( c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow=3, ncol=3) # From http://www.purplemath.com/modules/mtrxinvr2.htm

# No cache first time.
m <-  makeCacheMatrix(x)
cacheSolve(m)
m
# m = -24,  18,   5    
#      20  -15   -4
#      -5    4    1

# Retrieving form the cache the second time.
cacheSolve(m)
m
# Prints "Returning cached matrix"
# m = -24,  18,   5    
#      20  -15   -4
#      -5    4    1
