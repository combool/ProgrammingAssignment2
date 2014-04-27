## Put comments here that give an overall description of what your
## functions do
##This function which can be used to assign a value to an object in an environment 
##that is different from the current environment. Below are two functions 
##that are used to create a special object that stores a numeric vector and cache's its mean.
## Write a short comment describing this function
## makeCacheMatrix: function to provide cache for matrix inverse
## for example:## x <- makeCacheMatrix(matrix(matrixx))
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Write a short comment describing this function
##for example:x <- makeCacheMatrix(matrix(matrixx))
##    inverse <- cacheSolve(x)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
  }
}

