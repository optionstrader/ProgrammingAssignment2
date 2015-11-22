##
## First function creates a cache marix object 
## second function solves the matrix for inverse 
##

## Create a cacheMatrix object for a matrix
## Test Run
##      > source("cacheMatrix.R")
## Create x
##      > x = rbind(c(1, 2), c(2,1))
## Make Cache Matrix
##      > m = makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## Make sure m is created as expected
##      > m$get()
##           [,1] [,2]
##      [1,]    1    2
##      [2,]    2    1

## cacheSolve Returns a matrix that is the inverse of 'x' created above
## if inverse exists, gets results from cached data if not creates one
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

## Find Inverse of the matix with cacheSolve
##      > cacheSolve(m)
##                 [,1]       [,2]
##      [1,] -0.3333333  0.6666667
##      [2,]  0.6666667 -0.3333333
##      >
