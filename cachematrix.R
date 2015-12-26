## makeCacheMatrix: makes an object with methods to read and write values for the original matrix and its inverse
## cacheSolve: checks if an inverse exists in the object and creates an inverse if not

## creates an object with two variables and four methods: a reader and a writer for each of the two variables
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
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


## checks the cache for the inverse of a matrix and if none exists creates a new one
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

