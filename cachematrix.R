## In this R code file, we create a set of two functions: makeCacheMatrix and
## cacheSolve. The first function, when a matrix attribute is used, 
## creates a makeCacheMatrix object and then, when put through the second 
## function, cacheSolve,
## the inverse matrix is calculated and stored in a cache. This takes advantage
## of R's use of lexical scoping to save solutions that normally would take a
## long time for R to solve. 

## `makeCacheMatrix` creates a makeCacheMatrix object, which contains a list
## of four functions that are defined within the makeCacheMatrix function. One
## of these contains the cache environment, with values x and m. x is the matrix
## inputted into the function, while m is an empty value for the inverted matrix.
## The four functions retrieve the matrices and solve for the inverse matrix. 
## The functions themselves are not utilized until the object created in the
## makeCacheMatrix function is passed through the cacheSolve function.

## Most lines have a comment outlining what the following line of code does.

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL #empty value for the inverted matrix
  
  ##create a function that makes an environment within the makeCacheMatrix object
  set <- function(y) { 
    x <<- y ##the matrix from the function
    m <<- NULL #empty value for the inverted matrix
  }
  #A function to retrieve the matrix
  get <- function() x 
  
  ## A function for solving the inverse matrix. The <<- operator saves the inverse
  ## matrix into the cache.  
  setinversematrix <- function(solve) m <<- solve
  
  ## Function for retrieving the cached inverse matrix 
  getinversematrix <- function() m
  
  ## Runs the getinversematrix function
  getinversematrix
  
  ## Create a list of all the functions created above.
  ##The set environment will include the values x and m
  list(set = set, 
       get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## The `cacheSolve` function, defined below, calculates the inverse
## of the matrix that's stored in the makeCacheMatrix object. 
## It looks through any cached solution as well and returns the saved value
## if there is one. Otherwise, it calculates the inverse and caches it.

## The cacheSolve function only allows an input of a makeCacheMatrix object.
## It returns the matrix that's the inverse of the matrix x stored in the 
## environment of the makeCacheMatrix object.

## This takes advantage of "lexical scoping" - the saved inverse matrices 
## are stored within the "set" environment in the makeCacheMatrix object, and
## the cacheSolve function searches through the environment of 
## the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
  ## call the function "getinversematrix" within the makeCacheMatrix object
  m <- x$getinversematrix()
  
  ## if there is a nonmissing value in the 'm' object in the set environment,
  ## show the message that the function is retrieving the cached data.
  ## This message will only show the second time "cacheSolve" is run with
  ## the same "makeCacheMatrix" object.
  ## If there is a nonmissing value, the inverse matrix will be printed.
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #print the cached matrix
  }
  
  ## However, if there is no matrix in the cache, retrieve the matrix x in
  ## the set environment 
  data <- x$get()
  
  ## Solve for the inverse matrix 
  m <- solve(data) ##solve for the inverse matrix
  
  ## Save the inverse matrix to the cache 
  x$setinversematrix(m)
  
  ## Return the inverse matrix 
  m
}
