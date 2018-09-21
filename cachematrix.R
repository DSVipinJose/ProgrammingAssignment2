## Chaching the inverse of a Matrix

## The Below function will store a matrix and registers it inverse


#Capturing the Matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<-y
  inv<<- NULL
  }
  #inversing the matrix
get<- function() x
setInverse<- function(inverse) inv<<- inverse
getInverse<- function() inv
list(set=set,
     get=get,
     setInverse=setInverse,
     getInverse=getInverse)

}


## The below function computes the inverse of the  "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated  
## then it should display  the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("display cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}



#Working Example of function
#The result has been copied from the console
my_matrix<-makeCacheMatrix(matrix(1:4,2,2))
my_matrix$get()
        [,1] [,2]
[1,]    1    3
[2,]    2    4
my_matrix$getInverse()
NULL
cacheSolve(my_matrix)
       [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
cacheSolve(my_matrix)
display cached data
      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
my_matrix$getInverse()
      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5