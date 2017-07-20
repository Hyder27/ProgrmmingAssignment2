## Hyder Ali
## cachematrix.R

##	Example of Usage: source("cachematrix.R")
#> aSquareMatrix <- makeCacheMatrix(matrix(0:8, 8, 8))
#> squareMatrix$getMatrix()
#> aSquareMatrix$getCache() # will return NULL for the 1st time
#> cacheSolve(aSquareMatrix)
#> aSquareMatrix$getCache() # will return the solution

## makes a cache matrix from a given matrix

makeCacheMatrix <- function(x = matrix()) {

	cacheMatrix <- NULL

	setMatrix <- function(y) 
	{
		x<<-y
		cacheMatrix <<- NULL
		}
		
	getMatrix <- function() x
    
  setCache <- function(inverse) cacheMatrix <<- inverse

  
  getCache <- function() cacheMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)

}


## Return the inverse of a given matrix utilizing the cache

cacheSolve <- function(x, ...) {
  
  cacheMatrix <- x$getCache()
  
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }  
}
