## From assignment: This function creates a special "matrix" object that can cache its inverse.

## This function generates a list of functions.
## First, the function reads in a matrix as its input

makeCacheMatrix <- function(x = matrix()) {
        
        ## Define m and set it to empty
        
        m <- NULL
        
        ## define set as a function of an object y.
        ## 
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## create a function that retreives X
        
        get <- function() x
        
        ## create a function setinverse that is a function of the operator (inverse).  It will return
        ## m, the inverse of the matrix, if called in the next function (i.e. if the cachesolve function
        ## creates the inverse.) and store it as getinverse in its environment.
        
        setinverse <- function(solve) m <<- solve
        
        ## getinverse returns the inverse, m, if it exists.
        
        getinverse <- function() m
        
        ## list simply outputs all of the functions defiines in the operator
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function operates on the output of the makeCacheMatrix function.  The
## makeCacheFunction basically creates an environment where x and its inverse are stored,
## if the inverse has been calculated previously in a cacheSolve function.

cacheSolve <- function(x, ...) {
        
        ## Return m from the object defined by cacheSolve
        m <- x$getinverse()
        
        ## if m has been populated (i.e. if the inverse has been calculated already),
        ## then return m.  Display a message stating that the inverse has already been created.
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if m has not been populated, then get the matrix from the object created in
        ## cacheSolve and store it as the variable data.
        
        data <- x$get()
        
        ## define m as the inverse of the matrix
        
        m <- solve(data, ...)
        
        #run the set inverse function and save m in the object makeCacheMatrix
        
        x$setinverse(m)
        
        #output m
        
        m
}
