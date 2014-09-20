## Function makeCacheMatrix sets a matrix, gets the the matrix, 
## sets the inverse of the matrix, and gets the inverse of the matrix

## Function cacheSolve calculates the inverse of the matrix created with
## makeCacheMatrix. If the inverse is already calculated, it gets the inverse
## from the cashe. Otherwise it calculates the inverse and sets the inverse 
## in the cache via the setinv function

## sets and gets a matrix, sets and gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set  <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## retrieves existing inverse or computes the inverse of data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) & (x$get() == y) {
                message("getting cached data")
                return(m)
        } elseif (!is.null(m)) & (!x$get() == y){
                makeCacheMatrix(y)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
        
}
