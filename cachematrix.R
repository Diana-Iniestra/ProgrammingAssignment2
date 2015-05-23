
## This function is built to save a special 'matrix', which is really a list of functions.
makeCacheMatrix <- function(x = matrix()) {
	# 'inv' is the variable which would cache the inverse of the matrix. 
  # 'NULL' is the default value, as nothing is saved before about the matrix.
  inv<-NULL
  
  # This first function stores a matrix.  
	set<-function(y){
		x<<-y
		inv<<-NULL
	}

  # This second function returns the value of the matrix.
	get<-function() x

  # This third function stores the inverse of the matrix.
	setinverse<-function(inverse) inv<<-inverse

  # This fourth function returns the inverse of the matrix.
	getinverse<-function() inv

	list(set = set, get = get,
	     setinverse=setinverse,
	     getinverse=getinverse)
}



## This function is built to calculate the inverse of the special 'matrix' created with the above function.
## It'd check first if the inverse has already been calculated.
cacheSolve <- function(x, ...) {
	
  # This function gets the inverse of the matrix, if it has already been calculated. 
	inv<-x$getinverse()
	
  # If the inverse exists, this function would return that value.
  if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	# This functions gets the matrix.
  mat<-x$get()
	# This function calculates the inverse of the matrix and saves it in 'inv'.
  inv<-solve(mat)
  # This function sets the inverse if the matrix in the cache.
	x$setinverse(inv)
	inv
}
