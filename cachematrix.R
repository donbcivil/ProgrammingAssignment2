# Get And Cache Matrix Inverse - Don Baldwin 2 May, 2016
# The purpose of these functions is to cache solved matrix
# inverses, returning the cached version in subsequent calls.
#
# Example of use:
# 
## 1) Generate a matrix
# testm2 <- matrix(rnorm(100),10,10)
## N.B. We are assuming that the matrix is invertible. This could be
## tested by seeing if (det(testm2) == 0), reporting error with stop() if it is.
#
## 2) x2 <- makeCacheMatrix(testm2)
## 3) cacheSolve(x2) #solves for the inverse, caches it
## 4) cacheSolve(x2) #returns the cached version


## Create a list storing the get, set, getsolved () function and
## setsolved() function.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolved <- function(solve) m <<- solve
    getsolved <- function() m
    
    list(set = set, get = get,
         setsolved = setsolved,
         getsolved = getsolved)
}


## Returns a matrix that is the inverse of x. 
cacheSolve <- function(x, ...) {
    m <- x$getsolved()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolved(m)
    m
}
