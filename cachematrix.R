## Put comments here that give an overall description of what your
## functions do

## This function creates a list of the different functions related to cacheSolve

makeCacheMatrix <- function(x = numeric()) {

	  ##initialises inverse_matrix, the environment variable for the inverse matrix

    	  inverse_matrix <- NULL



	  ##creates the set function for the environment variable for the input matrix, in the case the cache has to be filled in

        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }


	  ##creates the get function for the environment variable for the input matrix, in the case the cache is already filled in

        get <- function() x



	  ##creates the set function for the environment variable for the output/inverse matrix, in the case the cache has to be filled in

        setinverse <- function(solve) inverse_matrix <<- solve



	  ##creates the get function for the environment variable for the output/inverse matrix, in the case the cache is already filled in

        getinverse <- function() inverse_matrix



	  ## creates the list of the different functions for setting/getting cache

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverted matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
	  ## Obtains the inverse matrix through the get function
        inverse_matrix <- x$getinverse()

	  ## Test if the inverse_matrix already exists - is not null
        if(!is.null(inverse_matrix)) {
                message("getting cached data")

		    ## Returns it if the inverse_matrix already exists
                return(inverse_matrix)
        }

	  ## Get the matrix in that is in the Cache
        source_matrix <- x$get()

	  ## Calculates the inverse of the source matrix
        inverse_matrix <- solve(source_matrix)

	  ## Set the calculated inverse as the inverse_matrix
        x$setinverse(inverse_matrix)

	  ## returns the inverse matrix
        inverse_matrix
}

