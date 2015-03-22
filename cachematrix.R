## Functions for inverting a matrix
## Saves the value in cache and prints the same if the matrix remains unchanged

## Defines the makeCacheMatrix function
## This function can create and cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) 
{
		## Initialize the inverse as NULL
		inv <- NULL
        
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        
        get <- function()
        {
                x
        }
        
		## Use the solve function to calculate inverse
        setinv <- function(solve)
        {
                inv <<- solve       
        }
        
        getinv <- function()
        {
                inv
        }
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Defines the cacheSolve function
## This function checks if the called matrix is the same as the one that was previously called
## If yes, it returns the cached inverse

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        
		## Check if cached inverse exists or not. If yes, return the same along with a message
        if(!is.null(inv))
        {
                message("Getting cached inverse")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}