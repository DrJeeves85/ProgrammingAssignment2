## This R-script returns the inverse of a given matrix using two functions:
## makeCacheMatrix and cacheSolve.  In order to obtain the inverse of a matrix
## x, run makeCacheMatrix first with x as an input.  Then, run cacheSolve with 
## the makeCacheMatrix object as input.


## makeCacheMatrix: builds a set of functions and returns the functions in a list
## to the parent environment.

makeCacheMatrix <- function(x = matrix()) {

	# INITIALIZE MATRIX OBJECT
	mat <- NULL

	# RESET ANY VALUE OF mat THAT WAS CACHED DUE TO PRIOR EXECUTION BASED ON OLD DATA
	set <- function(y) {
		x <<- y
		mat <<- NULL
	}

	# FUNCTION WHICH OBTAINS INPUT MATRIX
	get <- function() { x }

	# FUNCTION WHICH STORES SOLUTION IN CACHED mat
	setinv <- function(inverse) {mat <<- inverse}
	
	# FUNCTION WHICH OBTAINS THE CACHED mat
	getinv <- function() { mat }

	# LIST WITH THE FOUR FUNCTIONS TO BE RETURNED
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: Checks first whether a cached version of the inverse matrix.  If it does exist,
## then it returns that solution, otherwise it computes the inverse of the input matrix.

cacheSolve <- function(x, ...) {
        
	# GET CACHED VERSION OF INVERSED MATRIX
	mat <- x$getinv()

	# RETURN MAT IF MAT IS NOT NULL
	if (!is.null(mat)) {
		message("getting cached matrix")
		return(mat)
	}

	## If MAT is null, we compute it in the below
	
	# GET INITIAL MATRIX
	data <- x$get()

	# FIND INVERSE OF MATRIX
	mat <- solve(data)

	# CACHE MAT
	x$setinv(mat)

	mat

}
	


