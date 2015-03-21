## makeCacheMatrix is executed first, and its output is passed to cacheSolve.
## makeCacheMatrix outputs a list of functions, which are used by cacheSolve
## to retrieve the original input matrix and check if an inverse has already
## been stored in the cache. If it has, this is returned, and a message stating
## that cached data is being used is printed. If an inverse has not been
## cached yet, one is computed using the solve function, and then set and
## retrieved using the list of functions returned by makeCacheMatrix.

## This function, makeCacheMatrix, returns a list of functions that will be
## used by the other function, cacheSolve. Specifically, it creates functions
## to set and get the original matrix, as well as set and get the inverse to
## that original matrix. It takes one input, a matrix, which is empty by default.

makeCacheMatrix <- function(x = matrix()) {
        ## inv will be the inverse of the original matrix. This ensures it
        ## starts off as NULL, which will be checked by cacheSolve to see if
        ## a value for the inverse matrix has already been solved and cached.
        inv <- NULL
        
        ## Creates a function to set the variable x to this function's input.
        ## Also uses the superassignment operator to set m to NULL again, in
        ## case this function was used to change the value of the original
        ## matrix after an inverse to a previous matrix had already been cached.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Creates a very simple function that just returns the value of the
        ## input to the original makeCacheMatrix function.
        get <- function() x
        
        ## Creates a function that sets the inverse, inv, equal to the input
        ## to this function.
        setinverse <- function(solve) inv <<- solve
        
        ## Creates a simple function that returns the value of inv, the inverse
        ## matrix previously set by the setinverse function.
        getinverse <- function() inv
        
        ## Returns a list of functions, which can set and get the value of
        ## the initial matrix, and set and get the value of the inverse matrix.
        ## This list will be passed to cacheSolve.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes as an input the list of functions that is returned by
## makeCacheMatrix and uses them to create and cache an inverse matrix.

cacheSolve <- function(x, ...) {

        ## Assigns to m the value of the getinverse function within the
        ## makeCacheMatrix function, which is the variable inv. If a value for
        ## the inverse has not been cached, this will be NULL.
        m <- x$getinverse()
        
        ## Checks to see if there is already a cached value of the inverse.
        if(!is.null(m)) {
                ## If there is a cached value, it is returned and a message is
                ## output informing the user that this is cached data
                message("getting cached data")
                return(m)
        }
        
        ## Assigns to the variable data the result of the get() function from
        ## makeCacheMatrix, which is just the original matrix passed to
        ## makeCacheMatrix.
        data <- x$get()
        
        ## Runs the solve() function on the matrix called data in order to
        ## find the inverse of the matrix, and assigns the result to the
        ## variable m.
        m <- solve(data, ...)
        
        ## Calls the setinverse function within makeCacheMatrix on m, the
        ## result of running the solve function on the original matrix.
        x$setinverse(m)
        
        ## Returns the matrix m, which is the inverse of the original function.
        m
}
