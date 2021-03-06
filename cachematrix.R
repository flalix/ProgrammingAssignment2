## 1st set NULL to inv
## create set funcion, internally
## get returns x: the matrix
## setinv is a function that calcs the matrix inversion
## getinf is a function that returns inv
## at last all data is stored in a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## 1st: search for inv with getinv()
## if found: return inv
## else
## get data, solve, set the inverse
## return inv

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data for inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
}


#---------------------------------
install.packages("MASS")

library(MASS)

a = matrix(1:25, nrow=5, ncol=5)
a[2, 3] = 100
a[4, 4] = 99
a[2, 2] = 13
a[4, 5] = -1
a[5, 2] = 13


ginv(a) 

solve(a)

#--- here is the cache solve -------------
cacheSolve(makeCacheMatrix(a))



