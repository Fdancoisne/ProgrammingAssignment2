# as required in the assignment, these functions cache the inverse of a matrix

# Create a special matrix which is a list containing
# a function that
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<-y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<-inverse
	getinverse <- function() inv
	list( set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


# Calculate the inverse of the matrix created below, checking 
# first if it has already been calculated in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
