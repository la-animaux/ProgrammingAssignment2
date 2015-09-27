## This code contains two functions. The first function, makeCacheMatrix,
## creates a special 'matrix' object that can cache its inverse. The 
## second fucntion, cacheSolve, computes the inverse of a matrix. CacheSolve
## can save time by checking to see if the matrix inverse has already been
## created, and if so, retrieving those values from the computer's cache.

## makeCacheMatrix is a function that has two purposes. First, it creates 
## a special 'matrix' object. Second, it caches the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
    m = NULL  ##creates an empty object
    set = function(y) {  
        ##changes the vector stored in the main function
        x <<- y
           m <<- NULL
    }
    
    get <- function() x  
        ##returns the vector 'x' stored in the main function
    setinverse = function(inverse) m <<- inverse
        ##setinverse stores the value of the input in variable 'm' into the main function
    getinverse = function() m
        ##getinverse returns the value of the input
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ##this is a list of functions
}


## cacheSolve is a function that first checks if the inverse of a matrix 
## has been calculated and retrieves those values from the cache, and 
## second, it calcultaes the matrix inverse if necessary.

cacheSolve <- function(x, ...) {
    
## Return a matrix that is the inverse of 'x'
    
    m = x$getinverse()   ## assigns the value of getinverse to 'm'
    if(!is.null(m)) {  
        ## checks if 'm' is empty/null. If 'm' is not empty, returns 'm'
        message("getting cached data")
        return(m)
    }
    data <- x$get()  ## if 'm' is empty, create a new object 'data' that is empty
    m = solve(data, ...)  ## inverts the matrix and assigns it to 'm'
    x$setinverse(m) ## sets the value of the inverse to the cache
    return(m)   ##return the matrix
}
