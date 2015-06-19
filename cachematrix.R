## makeCacheMatrix is a function that creates a special "matrix" 
## object that can cache its inverse. cacheSolve is a function that  
## computes the inverse of the special "matrix" returned by the above 
## function.

## makeCacheMatrix is a function that stores a list of functions: set, 
## get, setinverse, getinverse. get is a function that returns the x 
## matrix stored in the main function. set is a function that changes 
## the matrix stored in the main function. setinverse and getinverse 
## store the value of the input in a variable m into the main function 
## makeCacheMatrix and return it.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<- y
    m<<- NULL
  }
  get<- function() x
  setinverse<- function(solve) m<<- solve
  getinverse<- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a function calculates the inverse of the special 
## "vector" created with the makeCacheMatrix: firstly, it checks to
## see if the inverse has already been calculated; ff so, it "get"s 
## the inverse from the cache and skips the computation; Otherwise, 
## it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
         message("getting cached data")
         return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
