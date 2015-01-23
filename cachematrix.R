## Here is a pair of functions that cache the inverse of a matrix
## For this assignment, assume that the matrix supplied is always invertible


## This function creates a special "matrix" object that can cache its inversion

makeCacheMatrix <- function(x = matrix()) {
	inversion <- NULL
	set <- function(y) {
		x <<- y
		mi <<- NULL
	}
	get <- function() x
	setInversion <- function(inv) inversion <<- inv
	getInversion <- function() inversion
	list(set = set, get = get,
		setInversion = setInversion,
		getInversion = getInversion)
}


## The following function calculates the matrix inversion for the matrix created 
## with the above function. However, it first checks to see if the matrix inversion
## has already been calculated. If so, it gets the value from the cache and skips 
## the computation. Otherwise, it calculates the inversion of the data and sets 
## the value in the cache via the setInversion function.

cacheSolve <- function(x, ...) {
	inversion <- x$getInversion()
	if(!is.null(inversion)) {
		message("getting cached data")
		return(inversion)
	}
	data <- x$get()
	inversion <- solve(data, ...)
	x$setInversion(inversion)

	## Return a matrix that is the inverse of 'x'
	inversion
}


## Simple test:

## 1. Create a random matrix
## > m <- matrix(rnorm(9),3)

## 2. Create the special matrix
## > cache_m <- makeCacheMatrix(m)

## 3. Calculate the inversion
## > cacheSolve(cache_m)

## 4. Return the cached inversion
## > cacheSolve(cache_m)
