## Following are the two functions as part of R-programming assignmentfor week 3. 
## The following new functions are based on the given example of "Caching the Mean of a Vector

## 1. "makeCacheMatrix": This function creates a special "matrix" object that can cache its inverse.

## "makeCacheMatrix" achieves following functionalities
## 1.1 set/save matrix in cache 
## 1.2 get/recalls that matrix
## and for the inverse of the original matrix
## 1.3 setInverse and 
## 1.4 getInverse 

makeCacheMatrix <- function(x = matrix()) 
  {
  m <- NULL
  set <- function(y)
    {
    x <<- y                                   ## "<<- used store in cache environment different from current"
    m <<- NULL                                ## "<<- used store in cache environment different from current"
    }
    get <- function() x                       ## recall matrix
    setInverse <- function(solve) m<<- solve  ## set inverse matrix
    getInverse <- function() m                ## get inverse matrix
    list(set = set, get = get,                ## list of functions is created here
         setInverse = setInverse,
         getInverse = getInverse)  
  }  


## 2. "cacheSolve": This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## "cacheSolve" achieves following functionalities (4 steps)
## 2.1 takes as input matrix created by the makeCacheMatrix function
## 2.2 checks to see if inverse is stored/available i.e. calculation is done
## 2.3 if done already, then recalls the inverse from cache
## 2.4 else it calculates matrix inverse and saves/stores it in the cache

cacheSolve <- function(x, ...) 
  {
                                              ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()                         ## check the cache
    if(!is.null(m))                           ## if something is present in the cache i.e. "not null" i.e. 
                                              ## the inverse has been previously calculated
      {
      message("getting cached data")          ## messaging that inverse is available in cache
      return(m)                               ## return the inverse from cache  
      }
      data <- x$get()                         ## if "null" i.e. is inverse is not calculated then get the matrix used in
                                              ## "makeCacheMatrix" function above
      m <- solve(data, ...)                   ## using "solve" function to calculate the inverse of the matrix
      x$setInverse(m)                         ## save/store the inverse matrix in cache
      m                                       
  }

