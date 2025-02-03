## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL  # Initialize inv (the cached inverse) to NULL
    
    set <- function(y) { # Setter function to set the matrix
        x <<- y        # Assign the new matrix to x in the parent environment
        inv <<- NULL   # Reset inv to NULL since the matrix has changed
    }
    
    get <- function() x  # Getter function to retrieve the matrix
    
    setinverse <- function(inverse) inv <<- inverse # Setter for the inverse
    getinverse <- function() inv # Getter for the inverse
    
    # Return a list of functions (the "special" matrix object)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
    
    inv <- x$getinverse() # Try to get the cached inverse
    
    if(!is.null(inv)) { # Check if the inverse is already cached
        message("getting cached data") # Print a message indicating cached data
        return(inv) # Return the cached inverse
    }
    
    # If the inverse is not cached, calculate it
    data <- x$get()  # Get the matrix from the makeCacheMatrix object
    inv <- solve(data) # Calculate the inverse using solve()
    x$setinverse(inv) # Cache the calculated inverse
    inv # Return the calculated inverse    
    
}
