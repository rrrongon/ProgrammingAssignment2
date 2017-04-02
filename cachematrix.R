## these two function tries to show the use cached property. here we set our computed value in cache and retrieve as long as any other value don't get set.

## makeCacheMatrix function creates a list containing some functions of
## setmatrix,get matrix, set calculated inverse matrix value and get calculated inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
	rev <- NULL
	set <- function(y){
		x <<- y
		rev <<- NULL
	}
	get <- function(){
		x
	}
  
	set_rev_matrix <- function(rev_matrix){
		rev <<- rev_matrix
	}
	get_rev_matrix <- function(){
		rev
	}
  
	list(set = set, get = get,
	set_rev_matrix = set_rev_matrix,
	get_rev_matrix = get_rev_matrix)

}


## cacheSolve function calculates the inverse of the matrix created with the above function. it first checks if the inverse has already been calculated. If so, it gets the result from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the set_rev_matrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	rev <- x$get_rev_matrix()
  	if(!is.null(rev)){
    		message("getting inverse matrix")
    		return(rev)
  	}
  
  	matrix_val <- x$get()
  	rev_matrix_val <- solve(matrix_val)
  	x$set_rev_matrix(rev_matrix_val)
  	rev_matrix_val
}
