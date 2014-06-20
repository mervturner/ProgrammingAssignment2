## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}	

## Write a short comment describing this function
## cacheSolve: 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
	
}

## Testing the functions

## Matrix inverse test
# create an invertible matrix
amatrix <- matrix(c(1, 2, 3, 4), nrow = 2,  ncol = 2)  
# display the matrix
amatrix
# show matrix is invertable if determinant not equal to zero
det(amatrix)
# compute the inverse of the matrix
inverse_matrix <- makeCacheMatrix(amatrix)
# display the inverse of the original matrix
inverse_matrix
# calculate the inverse - first time by calc
cacheSolve(inverse_matrix)
# calculate the inverse - by retrieving from cache
cacheSolve(inverse_matrix)


