## Example of lexical scoping using custom functions in R

## The 'makeCacheMatrix' function intializes an enviroment to return a list of 5 functions + passed matrix parameter
## The matrix is inverted using the solve() function and the output is stored as the variable 's'

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL ## initializes value
  set <- function(y) {  
    x <<- y  ## operator '<<-' sets y to x which can now be called outside the set function
    s <<- NULL 
  }
  get <- function() x  ## retrieves value of matrix passed through
  setinverse <- function(solve) s <<- solve ## calculates and stores the inverse of the passed matrix
  getinverse <- function() s ## returns the value of s
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## list of functions 

}


## This function computes the inverse of a given matrix if it does not exist already
## Otherwise the function will retrieve the inverse from cache 

cacheSolve <- function(x, ...) {
  s <- x$getinverse()  ## retrieves s for a given matrix
  if(!is.null(s)) {  ## if s is not a null value, then the inverse output is cached and can be returned
    message("getting cached data")
    return(s)
  }
  data <- x$get() ## if s is not in cache, then an inverse for it will be computed and cached
  s <- solve(data, ...) ## computing inverse with the solve function of R
  x$setinverse(s) ## s is cached 
  s
}
