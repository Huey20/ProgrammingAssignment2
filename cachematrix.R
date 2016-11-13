## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function (x = matrix()) {
  inver <- NULL
  set <- function (y) {
    x <<- y
    inver <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)
}

# Computes the inverse of the matrix create by makecachematrix. If the inverse has already been calculated, then retrieve from the cache
cacheSolve <- function(x,...) {
  inver <- x$getinverse()
  if(!is.null(inver)){
    message("getting cache data")
    return(inver)
  }
  datamatrix <- x$get()
  inver <- solve(datamatrix, ...)
  x$setinverse(inver)
  inver
}
