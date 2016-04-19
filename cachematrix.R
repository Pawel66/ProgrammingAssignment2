## Below is a pair of functions which compute and cache an inverse of a matrix




## This function creates a special object that can cache
## a matrix and its inverse
## It also contains functions which allow to set and get these caches

makeCacheMatrix <- function(x = matrix()) {
	inv_x <- NULL
	set <- function(y) {
		x <<- y
		inv_x <<- NULL
	}
	get <- function() x
	set_inv <- function(inv) inv_x <<- inv
	get_inv <- function() inv_x
	list( set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}




## This function computes inverse of the matrix which
## is stored in cache made by makeCachMatrix(). If the
## inverse exists in cache (has been calulated earlier)
## this functions gets the inverse from cache and
## returns as a result


cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
    inv_x <- x$get_inv()
    if (!is.null(inv_x)) {
    		message("getting cached data")
    		return(inv_x)
    }
    m_data <- x$get()
    inv_x <- solve(m_data)
    x$set_inv(inv_x)
    inv_x    
}
