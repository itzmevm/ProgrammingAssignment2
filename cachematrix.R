##Assignment : Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 

## makeCacheMatrix() and cacheSolve() functions are created here to solve 
## the costly computation challenges involved in inverting a matrix(non singular, i.e invertible)

## makeCacheMatrix function accepts an invertible matrix as input and creates a vector 
## which is really a list containing a function to

## 1) set the value of the vector
## 2) get the value of the vector
## 3) set the value of the solve(inverse) function  
## 4) get the value of the solve(inverse) function


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve 
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}

##Testing :
## NOTE : if even one value in the matric is NA, the result of this function will be 
## an all NA matrix
# 
# mtx <- matrix(c(1:8,0),3,3)
# 
# test <-makeCacheMatrix(mtx)
# 
# test$get()
# 
# cacheSolve(test)
# 
# mtx <- makeCacheMatrix(matrix(c(1:8,10),3,3))
# 
# test1 <- matrix(c(1:8,10),3,3)
# det(test11)


