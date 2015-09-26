## I just modified the the given example
## The makeCacheMatrix() function is infact a list of functions
makeCacheMatrix <- function(x = matrix()) {
 #Chekking the imput
	  if (is.matrix(x)==FALSE){
	   stop("Format is shit")}
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
	get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
## This function computes the inverse of a matrix.
## If it is a new matrix, then use solve(), to compute the inverse, otherwise it uses makeCacheMatrix() to catch the already computed inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
	#Chekking if the desired output already exist
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get() #this simply returns the original matrix
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
#EXAMPLE
#hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#A <- hilbert(4)
#> A
#          [,1]      [,2]      [,3]      [,4]
#[1,] 1.0000000 0.5000000 0.3333333 0.2500000
#[2,] 0.5000000 0.3333333 0.2500000 0.2000000
#[3,] 0.3333333 0.2500000 0.2000000 0.1666667
#[4,] 0.2500000 0.2000000 0.1666667 0.1428571
# > x<-makeCacheMatrix(A)
# > cacheSolve(x)
     # [,1]  [,2]  [,3]  [,4]
# [1,]   16  -120   240  -140
# [2,] -120  1200 -2700  1680
# [3,]  240 -2700  6480 -4200
# [4,] -140  1680 -4200  2800
# > 
# > cacheSolve(x) #THIS TIME THIS FUNCTION JUST RETRIEVES THE ALREADY COMPUTETED INVERSE
# getting cached data
     # [,1]  [,2]  [,3]  [,4]
# [1,]   16  -120   240  -140
# [2,] -120  1200 -2700  1680
# [3,]  240 -2700  6480 -4200
# [4,] -140  1680 -4200  2800
