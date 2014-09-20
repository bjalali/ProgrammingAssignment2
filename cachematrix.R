## Behrang Jalali, 20.09.2014
## This script computes an inverse matrix of a given invertible matrix, and  
## also checks if the inverse matrix is cached or need to be calculated.
## This script successfully passed the unit test by Gregory D. Horne.
## Thanks to many helpful discussions in the course forum.

## makeCacheMatrix sets up a list of four functions that can set and get the 
## inverse of a given matrix using cacheSolve function:
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL            # assigning Null to m (supposed to be the inverse)        
        set <- function(y) { # sets a matrix and super-assigns to x and m matrices
                             # so that they will be accessible outside of this environment
                x <<- y       
                m <<- NULL    
        }
        get <- function() x  # returns the original given matrix
        # below setinverse, super-assigns the inverse matrix to m:
        setinverse <- function(inverse) m <<- inverse 
        # below getinverse returns the inverse matrix:
        getinverse <- function() m    
        # returning a list of four functions including the original and inverse matrices
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}       


## cacheSolve returns the inverse matrix of the given matrix in makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$getinverse()  # assigns the inverse matrix or Null to m 
        # we check below if the inverse matrix (m) already is done:
        if(!is.null(m)) { # if yes, this returns the cached inverse matrix 
                message("getting cached data")
                return(m)
        }  
        # if not, inverse matrix will be calculated in the following section.
        # assigning the input matrix by makeCacheMatrix get function to data:
        data <- x$get()       
        # calculate the inverse of a matrix using the R function solve:
        m <- solve(data, ...) 
        # setinverse will super-assign the calculated inverse to m:
        x$setinverse(m)       
        m # Returning the inverse matrix of the given matrix
}

