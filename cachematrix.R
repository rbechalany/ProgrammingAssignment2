## This script is used to calculate the inverse of a matrix and store that
## inverse in the cache for quick access when it is required again.

## The script is comprised of 2 functions; the first of which is used to create
## a special matrix which has the ability to cache its inverse, and the second
## is used to calculate the inverse and cache it for future use (if the matrix
## has not changed)


## The below function creates a special matrix which can cache its inverse.
## It expects an input matrix argument and assumes that matrix is invertible.
makeCacheMatrix <- function(x = matrix()) {
        #Initialize the inverse of the matrix to NULL.
        invMatrix <- NULL
        #Create a sub function which sets the matrix of the special matrix.
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        #Create a sub function which returns the matrix.
        get <- function() x
        #Create a sub function which sets the inverse of a matrix (caches it).
        setInverseMatrix <- function(inverseMatrix) invMatrix <<- inverseMatrix
        #Create a sub function which gets the cached inverse of a matrix.
        getInverseMatrix <- function() invMatrix
        #Return a list which contains the 4 sub functions above.
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## The below function expects a special input matrix as an argument.
## It returns the cached matrix inverse if it exists, or calculates the inverse
## and caches it if it didn't already exist.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Get the cached matrix inverse if it exists
        invMatrix <- x$getInverseMatrix()
        #Return the cached matrix inverse if it exists
        if(!is.null(invMatrix)) {
                message("getting the cached matrix inverse")
                return(invMatrix)
        }
        
        #If the inverse matrix was not cached, it will be calculated and cached
        
        #Get the matrix from the special matrix
        originalMatrix <- x$get()
        #Calculate the inverse of the matrix (assuming the matrix is invertible)
        invMatrix <- solve(originalMatrix, ...)
        #Cache the matrix inverse
        x$setInverseMatrix(invMatrix)
        #Return the matrix inverse
        invMatrix
}

## Example for testing this script:
## specialMatrix <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(specialMatrix) #calculates the inverse and stores it in the cache.
## cacheSolve(specialMatrix) #calling it again retrieves it from the cache.