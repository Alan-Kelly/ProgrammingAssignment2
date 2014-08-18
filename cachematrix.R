## cachesolve is a function that returns the inverse of a matrix. 
## matrix  x is created by the function makematrix which must be passed a matrix 

makematrix <- function(x = matrix()) {  ## pass a matrix x into the function
  m <- NULL
  get <- function() x                   ## get is function that retrieves the matrix
  setsolve <- function(solve) m <<- solve(x)  ## assigns to m the result of solve
  getsolve <- function() m              ## function that retrieve m 
  list(get = get,                       ## creates a list
       setsolve = setsolve,
       getsolve = getsolve)
}
cachesolve <- function(x, ...) {    ## pass the result of makematrix to cachesolve
  m <- x$getsolve()                 ## check value of getsolve to see if solve has   
  if(!is.null(m)) {                 ## already been executed and read from cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()                   ## if not then get the matrix and calculate solve
  m <- solve(data, ...)
  x$setsolve(m)
  m                                  ## return the result of solve(data,...)
}