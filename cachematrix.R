## Put comments here that give an overall description of what your
## functions do

## Create a special set of functions that will allow us to store an 
## inverse memory in the cache

makeCacheMatrix <- function(x = matrix()) {
	## keep an internal copy of the inverted matrix
	i <- NULL

	## store the matrix in this scope
	seti <- function(y) {
		x <<- y
		i <<- NULL
	}

	## retrieve the matrix from this scope
	geti <- function() x

	## store the inverse matrix in this scope
	setinverse <- function(inv) i <<- inv

	## retrieve the inverse matrix from this scope
	getinverse <- function() i

	## associate these functions with a list so they can be called
	list(seti = seti, geti = geti, 
		setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse of a matrix, from cache if it's already there 
## and calculated if not 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	## Try to retrieve the inverse from cache and return it if it exists
	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	## If the inverse doesn't exist in the cache, calculate it and 
	## store it in the cache
	data <- x$geti()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

