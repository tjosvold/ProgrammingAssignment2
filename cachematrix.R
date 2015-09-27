## Put comments here that give an overall description of what your
## functions do

## function to create an object that will screate a makeCacheMatrix object
# this will cache the inverse as part of the object

makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
        get <- function() x
        setinverse <- function(ginv) m <<- ginv
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## function to retrieve the cached matrix inverse (if any)
# if this is the first time to compute the matrix inverse, the
# matrix inverse will be computed and then saved to the object

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	data <- x$get()
	m <- ginv(data)
	x$setinverse(m)
	m

}
