## Cache the inverse of a matrix
## Caching saves computation

## function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x<<-y
		m<<-NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)


}


## function computes inverse of a matrix created above. If the inverse was already
## calculated, retrieves it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting the cached data")
		return(m)
	}
	mat <- x$get()
	m <- solve(mat, ...)
	x$setInverse(m)
	m
}
