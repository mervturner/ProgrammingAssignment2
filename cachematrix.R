# TODO: CacheMatrix.R
# R Programming - Programming Assignment 2
# 
# Author: MLT
###############################################################################


## R function to cache potentially time-consuming computations
## For costly computations there is a benefit to caching the reult
## especially of teh object does not change its values
## The following pair of functions cache the inverse of a matrix

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse
## Returns a list of functions to be used for caching

makeCacheMatrix <- function(x = matrix()) {
	# define the caching functions
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	# return the caching functions as a list
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}	

## cacheSolve: 
## This function computes the inverse of the special "matrix" 
## using the makeCacheMatrix functions above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	# check to see if the result is cached
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	# if not cached, the caculate the reult and cache
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
	
}

## Testing the functions

### Matrix inverse test
## create an invertible matrix
#amatrix <- matrix(c(1, 2, 3, 4), nrow = 2,  ncol = 2)  
## display the matrix
#amatrix
#			# matrix value
#			#	   [,1] [,2]
#			# [1,]    1    3
#			# [2,]    2    4
## show matrix is invertable if determinant not equal to zero
#det(amatrix)
#			# determinant is non zero
#			# [1] -2
#
## compute the cache functions for the inverse of the matrix
#inverse_matrix <- makeCacheMatrix(amatrix)
## display the cache functions
#inverse_matrix
#			# makeCacheMatrix return steh list of functions
#			#$set
#			#function (y) 
#			#{
#			#	x <<- y
#			#	m <<- NULL
#			#}
#			#<environment: 0x00000000069c3bf8>
#			#		
#			#		$get
#			#function () 
#			#	x
#			#<environment: 0x00000000069c3bf8>
#			#		
#			#		$setinverse
#			#function (inverse) 
#			#	m <<- inverse
#			#<environment: 0x00000000069c3bf8>
#			#		
#			#		$getinverse
#			#function () 
#			#	m
#			#<environment: 0x00000000069c3bf8>
#		
#
## calculate the inverse - first time by calc
#cacheSolve(inverse_matrix)
#			# Calculation is performed
#			#	  [,1] [,2]
#			#[1,]   -2  1.5
#			#[2,]    1 -0.5
#
## calculate the inverse - by retrieving from cache
#cacheSolve(inverse_matrix)
#			# Subsequent calculations take result from cache
#			#...getting cached data
#			#	  [,1] [,2]
#			#[1,]   -2  1.5
#			#[2,]    1 -0.5
