## Assignment 2 R programming Lexical Scoping
## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
## inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve should retrieve the inverse from the cache.
##
##
## Usage Example and description
## create a 3x3 matrix called myMatrix 
## myMatrix <- matrix(c(0,1,2,1,0,2,1,2,0), nrow=3, ncol=3)
##
## to calculate the inverse of myMatrix use solve(myMatrix)
## solve(myMatrix)
##
## which will retutn 
##           [,1]       [,2]       [,3]
##      [1,] -0.6666667  0.3333333  0.3333333
##      [2,]  0.6666667 -0.3333333  0.1666667
##      [3,]  0.3333333  0.3333333 -0.1666667
##
## Every time the inverse of myMatrix is required, it will have to be recalculated or 
## calculated once and stored in a variable. This is good, but a more elegant solution
## is to creat a special object that will store the matrix and the inverse of that matrix
## 
## myCachedMatrix <-makeCacheMatrix(myMatrix)
##
## myCacheMatrix is now an object which holds myMatrix via the set function, myInverse is
## is set to NULL because we have not calculated the inverse yet
##
## myCachedMatrix$get() will return the input myMatrix
## 
## to calculate the inverse of the myCachedMatrix, we use the command :
##
## cacheSolve(myCachedMatrix)
##
## it calculates the inverse of the matrix and stores it into the variable 
## myInverse in the object myCachedMatrix.  It will also print the inverse on the screen due
## due to R auto print
##
##
## if somewhere do the line we need the inverse of myCahcedMatrix again, we can use
##  cacheSolve(myCachedMatrix).
## since the inverse has already been calculated, we will get this "getting cached data"
## and we will get the stored inverse of the matrix without recalculating it.
## 



## makeCacheMatrix() creates a special object which holds a matrix and the inverse of it
##
## Methods :
## makeCacheMatrix$set  is executed automatically when makeCacheMatrix is called is assigns 
##                      the input matrix to makeCacheMatrix
## makeCacheMatrix$get  returns the original matrix that was "assigned" when the set
##                      method was executed
## makeCacheMatrix$setInverse is used to "store" the inverse of the input matrix
## makeCacheMatrix$getInverse is used to retrieve the stored inverse of the input matrix
##
## Variables :
## myInverse is used to store the inverse of the input matrix x
## 
## function(x = matrix()) -> forces x to me ot class matrix, if x is not of class matrix
##    the function will not execute, it will cause an error right at the beginning



makeCacheMatrix <- function(x = matrix()) {
  ## creates an object that will store the original matrix (x) and the inverse of it
	myInverse <- NULL
	set <- function(y) {
		  x <<- y
		  myInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) myInverse <<- solve
	getInverse <- function() myInverse
	list(set = set, get = get,
	   setInverse = setInverse,
	   getInverse = getInverse)
}



## cacheSolve() actually calculates the inverse of the matrix and returns it
## myInverse <- x$getInverse()  gets the inverse from makeCacheMatrix$getInverse()
##
## if the variable myInverse is not null, it will return myInverse as the inverse of 
## the matrix it will not calculate it.  This is why myInverse is initially set to NULL in 
## makeCacheMatrix.
## if the variable myInverse is NULL, it will calculate the inverse of the matrix using solve()
## one thing to note
## data <- x$get()  actually calls the makeCacheMatrix$get to return the input matrix
## myInverse <- solve(data, ...) actually calculates the inverse of the matrix and assigns it to 
## myInverse
## x$setInverse(myInverse)  -> stores the inverse into myInverse of makeCacheMatrix via 
## makeCacheMatrix$setInverse
## x in that function is referring to makeCacheMatrix 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      myInverse <- x$getInverse()
      if(!is.null(myInverse)) {
          message("getting cached data")
          return(myInverse)
      }
      data <- x$get()
      myInverse <- solve(data, ...)
      x$setInverse(myInverse)
      myInverse
}
