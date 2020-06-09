## These functions solve the inverse of a matrix and store the solution to 
## memory. If the user wants to recall that information, the functions will
## check to see if the matrix has changed, and if not, will pull the solution
## from memory rather than re-compute.

## This function creates a matrix-like object with methods available to set and
## get the matrix value to/from global environment, and can do the same with the
## solved matrix as needed.

makeCacheMatrix <- function(x = matrix()) {
        inverse_mat <- NULL
        set <- function(y) {
                x <<- y
                inverse_mat <<- NULL
        }
        get <- function() x
        set_solve <- function(solved_mat) inverse_mat <<- solved_mat
        get_solve <- function() inverse_mat
        list(set = set, get = get, set_solve = set_solve, get_solve = get_solve)
}


## This function checks to see if the desired matrix and solution is already in 
## memory and pulls from that location if it all exists and is unchanged 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed_mat <- x$get_solve()
        stored_mat <- x$get()
        if(!is.null(inversed_mat)) {
                message("Matrix has solution. Pulling from cache")
                return(inversed_mat)
        } else {
                message("New matrix, solving and caching")
                data <- x$get()
                inversed_mat <- solve(data)
                x$set_solve(inversed_mat)
                inversed_mat
        }
        
}
