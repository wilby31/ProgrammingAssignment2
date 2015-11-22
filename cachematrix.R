## The following two functions are designed to cache a matrix of values
## such that future computation time of those values is drastically reduced

## makeCacheMatrix() sets a cached matrix in a different environment
## Once cached, any operation performed more than once on this data 
## will have dramatically reduced computational time

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve() leverages the solve() function used to calculate the inverse of 
## a matrix. The difference is that it caches the output of the function and utilizes a
## matrix sourced from makeCacheMatrix() 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

## TEST FUNCTIONS

## Calculate Time savings of finding inverse of 4M uniform 
## random values in 2k x 2k matrix on my laptop

system.time(A <- matrix(runif(4000000), 2000, 2000))  #Elapsed Time: 0.22 seconds
system.time(A_CacheMatrix <- makeCacheMatrix(A))      #Elapsed Time: 0.00 seconds
system.time(cacheSolve(A_CacheMatrix))                #Elapsed Time: 7.63 seconds (first time through)
system.time(cacheSolve(A_CacheMatrix))                #Elapsed Time: 0.00 seconds (after it has been cached)
system.time(solve(A))                                 #Elapsed Time: 7.47 seconds

