## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### 1) This function returns a new object that has accessors: 
#### set,get - for the current matrix being used (initially equal to x)  
#### setinverse,getinverse (for the cached inverse of the current matrix).
makeCacheMatrix <- function(x = matrix()) {
  cache <- x
  set <- function(newmatrix) {
    x <<- newmatrix
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
### 1) This function returns an inverse of the current matrix in the object 
### (see function makeCacheMmatrix above).
### 2) It will return cached value in the object if it isn't null.
### 3) Otherwise, it will calculate the inverse directly, cache it into the object 
### and return it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  curinv <- x$getinverse()
  if(!is.null(curinv)) {
    message("getting cached data")
    return(curinv)
  }
  data <- x$get()
  curinv <- solve(data, ...) # out of luck, cache was empty :(
  x$setinv(curinv) # calculate it directly and cache it into x 
  curinv
}
