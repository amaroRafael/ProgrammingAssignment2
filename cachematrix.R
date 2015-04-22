## Computes the inverse of the "matrix"
## if the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

## This function creates a speacial "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	mi <- NULL
	set <- function(y){
		x <<- y
		mi <<- NULL
	}
	get <- function() x
	setinverse <- function(minverse) mi < minverse
	getinverse <- function() mi
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## Computing the inverse of a square matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        mi <- x$getinverse()
        if (!is.null(mi)) {
        	return(mi)
        }
        m <- x$get()
        
        d <- det(m)
        
        if(d > 0) {
        	mi <- solve(m)
        	x$setinverse(mi)
        }
        
        mi
}
