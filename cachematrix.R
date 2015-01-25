## 
## R script file for cachematrix.R
##
## makeCacheMatrix creates a special "matrix", which is a list of 4 functions to:
##   a) set/get the matrix, 
##   b) set/get the inverse of the matrix
##
## cacheSolve returns the inverse of the stored matrix.  It will return the cached inverse; 
##   otherwise, the inverse will be calculated.

## equalMatrix is a helper function to compare 2 matrix and return T OR Function

equalMatrix <- function(e,f) {
	is.matrix(e) && is.matrix(f) && dim(e) == dim(f) && all(e == f)
} 

## makeCacheMatrix creates a list of functions that returns a list of 4 function elements:
##   1. set - function to set the matrix
##   2. get - function to get the matrix
##   3. setInverse - function to set the inverse of the matrix
##   4. getInverse - function to get the inverse of the matrix
## 
makeCacheMatrix <- function(x = matrix()) {
	## initialize variable
	m <- NULL
	
	## store matrix and its inverse
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## retrieve matrix
	get <- function() x
	
	## store matrix inverse into storage
	setInverse <- function(inv) m <<- inv
	
	## retrieve calculated inverse
	getInverse <- function() m
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special matrix created by makeCacheMatrix 
## Function firsts check if the inverse is already calculated.  If so, the cache version is returned.  
## If not, the inverse will be calculated.

cacheSolve <- function(x, ...) {
	
	## Retrieve current copy of matrix
	data <- x$get()
	
	m <- x$getInverse()
	if (!is.null(m)) {
		## if inverse is cached, return cache
		message("getting cached inverse")
	  return (m)
	}
	
	## Calculate new copy of inverse
	m <- solve(data, ...)
	x$setInverse(m)
	return (m)
}

