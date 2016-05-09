## Two parts 
## 1: Assign a chunk of memory as a matrix (the cached space to be referenced)
## 2: Return the cached value (inverse matrix), or calculate and return if it
##    has not yet been assigned a value.

## Assigns a chunk of memory to be used as

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL											# Assigns 'inv' as NULL (placeholder) 
	set <- function(y){									# defines 'set' in the top level environment as the matrix 'x' above, 'inv' as NULL
		x <<- y
		inv <<- NULL
	}
	get <- function() x									# defines 'get' as x - our original matrix
	setinv <- function(solve) inv <<- solve						# defines 'setinv' as the function solve(inv) to calculate the inverse of x
	getinv <- function() inv								# defines 'getinv' as inv - the actual inverse matrix
	LST<<-list(set = set, get = get, setinv = setinv, getinv = getinv)	# creates a cached list of (set, get, setinv, getinv)
}													# For reference in the comments below, this list is LST


## returns the inverse of matrix x - either the cached value or calculated on the spot and assigned to the cache

cacheSolve <- function(x, ...) {
	inv <- LST$getinv()				# assignes to 'inv' the value defined in LST as 'getinv'
								# This is our desired inverse matrix (if it exists or not will be handled below)
	if(!is.null(inv)) {				# Checks if inv has a value (namely our inverse matrix) assigned to it
		message("Getting Cached Data...")	# Friendly little note so you know this is cached data, not calculated
		return(inv)					# Spits out 'inv' which is our inverse matrix since is.null(inv)=FALSE
	}
	data <- LST$get()					# Assigns to data the value of get from LST - this is our original matrix
	inv <- solve(data,...)				# Calculates and assignes to 'inv' the inverse we're looking for
	LST$setinv(inv)					# Defines the cached value as our inverse
	inv							# Returns the inverse we wanted to see in the first place        
}								# Note that without the message("...") part, we know this was calculated


