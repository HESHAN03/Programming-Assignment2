makeCachematrix <- fuction(x=matrix()) {
  inv <- NULLset <- fuction (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() (x)
  setInverse <= fuction(inverse) (inv <<- inverse)
  gerInverse <- function() (inv)
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

cachesolve <- function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}