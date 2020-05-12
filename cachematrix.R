#
# My Programming Assignment 2 of the course "R Programming" by Johns Hopkins University in Coursera.
#
# The objetive is write a pair of functions that cache the inverse of a matrix
# by storing values in local environment of special cacheable matrix implementation.
# For this code, we assume that the matrix supplied is always invertible.
#
# The first function "makeCacheMatrix" below creates a cacheable matrix and return a list of four methods:
#
# 1 - get() - to get the value of the matrix
# 2 - set(matrix_invertible) - to set the value of the matrix and reset earlier calculated inverse
# 3 - getInverse() - to get the value of the cached inverse matrix or "NULL" if no cached inverse is present
# 4 - setInverse(matrix_invertible) - to set cached inverse matrix
#

makeCacheMatrix <- function(x = matrix()) {
  
  ivs_m <- NULL #create a cache variable and set it to NULL
  
  set <- function(y) { #set a new matrix
    
    x <<- y # assign a value to a variable in the parent enviroment. 
    ivs_m <<- NULL # reset stored cache
  }
  
  get <- function() x # get the matrix by returning the function arg "x"
  
  
  setInverse <- function(inverse) ivs_m <<- inverse #storing the matrix inverse in the cache variable
  
  getInverse <- function() ivs_m #return the cached inverse matrix
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) #list of mathods the the function will return
  
}

#
# The second function "cacheSolve" below calculates the matrix inverse 
# created with the "makeCacheMatrix" and also stores the value.
# However, it first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse matrix of the data and 
# sets the value of the inverse in the cache via the setInverse function.
#

cacheSolve <- function(x, ...) {
  
  ivs_m <- x$getInverse() # get cached inverse matrix
  
  if(!is.null(ivs_m)) { # if the cache is no null, it was calculated earlier
    message("getting cached data") 
    return(ivs_m) # return the inverse matrix
  }
  
  data <- x$get() # take the matrix out of cachable matrix
  inv_m <- solve(data, ...) # calculate the inverse matrix with the solve function 
  x$setInverse(inv_m) #set the result in the variable
  
  return(inv_m) # Return a matrix that is the inverse of 'x'
  
}

#
# How to use the functions:
#
# 1 - Pass the function makeCacheMatrix with the matrix invertible as an argument to the 
# funciton cacheSolve, like that:
# cacheSolve(makeCacheMatrix(x))
#
# 2 - Assign the result of the funtion makeCacheMatrix to a variable and than 
# pass it to cacheSolve as an argument, like that:
# z <- makeCacheMatrix(x)
# cacheSolve(z)
#
# Example:
# x <- matrix(c(4,3,3,2),2,2)
# z <- makeCacheMatrix(x)
# cacheSolve(z)

# Results:
#     [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
#
# If try again:
# cacheSolve(z)
#
# Then the cachesolve will retrieve the inverse from the cache.
# Will appear
# getting cached data
#     [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
#
