## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(invmtrx) m <<- invmtrx
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

## This is the function used to test the caching

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

## To run the test:
## source('cachematrix.R')
## set.seed(1110201)
## r = rnorm(1000000)
## mat1 = matrix(r, nrow=1000, ncol=1000)
## test(mat1)