## First we create a matrix (set and get a value of it) and than we
## calculate (set and get the value of) inverse matrix. On the second step we 
## get the inverse matrix which has been calculated. If it's not calculated
## yet, we calculate inverse matrix on this step.

## makeCacheMatrix create a matrix and than calculate and cache inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y, nrow, ncol){
		x<<-matrix(y, nrow, ncol)
		m<<-NULL
	}
	get<-function() x
	setsolve<-function(solve) m <<-solve
	getsolve<-function() m
	list (set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## With cacheSolve function we create inverse matrix. If the inverse matrix 
## has been calculated before, then cacheSolve function only return result from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
	        if(!is.null(m)) {
	                message("getting cached data")
	                return(m)
	        }
	        data <- x$get()
	        m <- solve(data, ...)
	        x$setsolve(m)
        m
}
