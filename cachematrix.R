## The idea is if we calculated the inverse of the matrix we don't calculate it again. So, we are intrested in cashing the matrix and its inverse.
## if the matrix does not exist then we calculate the inverse.

## the makeCacheMatrix function cashes the input matrix (x) and its inverse matrix (m) by creating "special" matrix. 
## The set and get functions are defined for both matrices (x and m).
makeCacheMatrix <- function(x = matrix()) {
	
      	m <- NULL
        set <- function(y) { #set the input matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x # get the input matrix
        setsolve <- function(solve) m <<- solve # set the inversed matrix
        getsolve <- function() m # get the inversed matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates the inverse of the special matrix created with the above function. 
## At first it checks if the associated inverse matrix to the input matrix has been calculated before. 
## If yes, then returns the cashed inverse matrix. Otherwise, it calculates the inverse of the matrix and cashes the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	      m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # clculate the inverse matrix of the data
        x$setsolve(m)
        m
}
