## This function creates a list of functions. By using set function, it creates
## a function for you, so you don't need to specify matrix before-hand.
## I've found that solve works only with small square matrices.
## So, be careful, i guess.

makeCacheMatrix <- function(x = numeric(), nrow, ncol) {
    m <- NULL
    set <- function(y, nrow, ncol) {
        x <<- matrix(y, nrow, ncol)
        m <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) m <<- Inv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Gets an inverse of the matrix. If the inverse was already calculated, i take
## it from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
